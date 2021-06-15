# Read data for the whole EXPANSE area
# For multiple-year modelling where different predictor values are used
library(dplyr)
library(raster)
seed <- 123
local_crs <- CRS("+init=EPSG:3035")
# target_poll = 'NO2'
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
read_APdata <- function(target_poll){
   
   # Read in station metadata
   # sta <- read.csv("../EXPANSE_predictor/data/processed/airbase_station_climate.csv")
   sta <- read.csv("../EXPANSE_predictor//data/processed/all_sta_climate.csv")
   # There are 3 missing rows and 7 rows with unknown values for sta_type
   # Remove them
   sta <- sta %>% filter(sta_type!="Unknown", sta_type!="")
   # Read in airbase data
   # airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_checked.csv")
   # airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_pollutants4_day_hr.csv")
   airbase <- read.csv("../EXPANSE_APM/data/processed/all_conc4.csv")
   no2 <- airbase %>% filter(component_caption==target_poll)
   # no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value, sta_code=station_european_code)
   # str(no2)
   # str(sta)
   no2_sta <- inner_join(no2, sta %>% dplyr::select("sta_code", "zoneID"), by="sta_code") 
   # the amount of data decreases because some stations with unkown station types are removed
   
   # Read in predictor values
   road <- read.csv("../EXPANSE_predictor/data/processed/road_merged.csv")
   
   macc <- read.csv("../EXPANSE_predictor/data/raw/gee/maccAndPop.csv")
   macc <- macc %>% rename(sta_code=station_european_code) %>% 
      dplyr::select(sta_code, pop, macc)
   
   pred <- inner_join(road, macc, by="sta_code")
   
   no2_e_all <- inner_join(no2_sta, pred, by="sta_code")
   # rm(road, macc, pred, sta, airbase, no2, no2_sta)
   no2_e_all
}
   

## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target, target_poll='NO2'){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   
   # Read in met data
   met <- read.csv("../EXPANSE_predictor/data/raw/gee/met_00_19.csv")  # Met data from GEE
   met <- met %>% 
      dplyr::select(-".geo", -"system.index") %>%
      rename(sta_code=station_european_code)
   met_yr <- met %>% names() %>% 
      strsplit(., "_") %>% 
      lapply(.,  `[[`, 2) %>% 
      unlist() %>% as.numeric()
   met_yr[is.na(met_yr)] <- 0
   
   # Read in omi data
   omi <- read.csv("../EXPANSE_predictor/data/processed/omi_expanse_2005_2012.csv")
   sat_pm25 <- read.csv("../EXPANSE_predictor/data/processed/satPM25_expanse_2001_2018.csv")
   
   
   # Read in linear interpolated lc values
   lc_filenames <- list.files("../EXPANSE_predictor/data/processed/linear_inter_lc/", "pred")
   lc_names <- gsub(".csv", "", lc_filenames)
   
   
   lc_years <- as.numeric(substr( list.files("../EXPANSE_predictor/data/processed/linear_inter_lc/", "pred"), 6, 9))
   met_years <- unique(met_yr)
   omi_years <- as.numeric(substr(names(omi)[grepl('omi_', names(omi))], 5, 9))
   sat_pm25_years <- as.numeric(gsub('satPM25_', '', names(sat_pm25 %>% dplyr::select(-sta_code))))
   
   # Single year
   if(length(yr_target)==1){
      # lc
      if(all(paste0("pred_", yr_target)%in%lc_names)){
         lc <- read.csv(paste0("../EXPANSE_predictor/data/processed/linear_inter_lc/pred_", yr_target,".csv"))
      }else{
         yr_target_close <- lc_years[which.min(abs(lc_years-yr_target))]
         lc <- read.csv(paste0("../EXPANSE_predictor/data/processed/linear_inter_lc/pred_", yr_target_close,".csv"))
      }
      no2_e_sub <- inner_join(no2_e_sub, lc[, c('sta_code', names(lc)[!names(lc)%in%names(no2_e_sub)])], 
                              by='sta_code')
      
      # Add omi (if the year include omi)
      if(target_poll=='NO2'){
         if(all(paste0("omi_", yr_target)%in%names(omi))){
            omi_target <- omi[, c("sta_code",paste0("omi_",yr_target))] %>% 
               rename(omi=paste0("omi_",yr_target))
         }else{
            yr_target_close <- omi_years[which.min(abs(omi_years-yr_target))]
            omi_target <- omi[, c("sta_code",paste0("omi_",yr_target_close))] %>% 
               rename(omi=paste0("omi_",yr_target_close))
         }
         no2_e_sub <- inner_join(no2_e_sub, omi_target, by="sta_code")
      }else if(target_poll=='PM2.5'){
         if(all(paste0("satPM25_", yr_target)%in%names(sat_pm25))){
            sat_target <- sat_pm25[, c("sta_code",paste0("satPM25_",yr_target))] %>% 
               rename(sat_pm25=paste0("satPM25_",yr_target))
         }else{
            yr_target_close <- sat_pm25_years[which.min(abs(sat_pm25_years-yr_target))]
            sat_target <- sat_pm25[, c("sta_code",paste0("satPM25_",yr_target_close))] %>% 
               rename(sat_pm25=paste0("satPM25_",yr_target_close))
         }
         no2_e_sub <- inner_join(no2_e_sub, sat_target, by="sta_code")
         no2_e_sub <- no2_e_sub[!is.na(no2_e_sub$sat_pm25), ]
      }
      # Add met
      if(all(yr_target%in%met_yr)){
         met_target <- met[, met_yr==yr_target]
         # names(met_target) <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist()
      }else{
         yr_target_close <- met_years[which.min(abs(met_years-yr_target))]
         met_target <- met[, met_yr==yr_target]
      }
      met_target$sta_code <- met$sta_code
      no2_e_sub <- inner_join(no2_e_sub, met_target, by="sta_code")
   }else{
      # Multiple years
      # lc
      # target_yrs <- no2_e_sub$year
      lc_list <- lapply(yr_target, function(yr){
         if(yr%in%lc_years){
            yr_target_close <- yr
         }else{
            yr_target_close <- lc_years[which.min(abs(lc_years-yr))]
         }
         lc <- read.csv(paste0("../EXPANSE_predictor/data/processed/linear_inter_lc/pred_", yr_target_close,".csv"))
         return(lc)
      })
      names(lc_list) <- paste0("pred_", yr_target)
      # lc_str <- paste0("pred_", target_yrs)
      lc_target <- lapply(seq_along(yr_target), 
                          function(yr_i){
                             lc_target_yr <- lc_list[[paste0('pred_', yr_target[yr_i])]]
                             inner_join(no2_e_sub %>% filter(year==yr_target[yr_i]) , 
                                        lc_target_yr[, c(names(lc_target_yr)[!names(lc_target_yr)%in%names(no2_e_sub)], 'sta_code')], 
                                        by='sta_code')
                          }
      ) %>% do.call(rbind, .)
      
      no2_e_sub <- lc_target
      
      # Add omi
      if(target_poll=='NO2'){
         omi_list <- lapply(yr_target, function(yr){
            if(yr%in%omi_years){
               yr_target_close <- yr   
            }else{
               yr_target_close <- omi_years[which.min(abs(omi_years-yr))]
            }
            as.data.frame(omi[, c('sta_code', paste0('omi_', yr_target_close))]) %>% 
               rename(omi=paste0('omi_', yr_target_close))
         })
         # omi_target <- omi[, paste0("omi_",yr_target)]
         names(omi_list) <- paste0('omi_', yr_target)
         
         
         no2_e_sub <- lapply(seq_along(yr_target), 
                             function(yr_i){
                                omi_target_yr <- omi_list[[paste0('omi_', yr_target[yr_i])]]
                                inner_join(no2_e_sub %>% filter(year==yr_target[yr_i]) , 
                                           omi_target_yr, 
                                           by='sta_code')
                             }
         ) %>% do.call(rbind, .)
         # omi_var <- 'omi' #lapply(strsplit(names(omi_target), "_"), `[[`, 1) %>% unlist %>% unique
         
         
         # omi_str <- lapply(omi_var, function(omi_str) paste0(omi_str, "_", no2_e_sub$year))
         # no2_e_sub_s <- inner_join(no2_e_sub, omi, "sta_code")
         # omi_target <- sapply(seq_along(omi_str), 
         #                      function(str_i){
         #                         sapply(seq_along(omi_str[[str_i]]), function(i) no2_e_sub_s[i, omi_str[[str_i]][i] ])
         #                      }
         # ) %>% as.data.frame()
         # names(omi_target) <- omi_var
         # # identical(no2_e_sub_s$sta_code, no2_e_sub$sta_code)
         # no2_e_sub <- cbind(no2_e_sub, omi_target)
      }else if(target_poll=='PM2.5'){
         sat_list <- lapply(yr_target, function(yr){
            if(yr%in%sat_pm25_years){
               yr_target_close <- yr   
            }else{
               yr_target_close <- sat_pm25_years[which.min(abs(sat_pm25_years-yr))]
            }
            as.data.frame(sat_pm25[, c('sta_code', paste0('satPM25_', yr_target_close))]) %>% 
               rename(sat_pm25=paste0('satPM25_', yr_target_close))
         })
         
         names(sat_list) <- paste0('satPM25_', yr_target)
         no2_e_sub <- lapply(seq_along(yr_target), 
                              function(yr_i){
                                 sat_target_yr <- sat_list[[paste0('satPM25_', yr_target[yr_i])]]
                                 inner_join(no2_e_sub %>% filter(year==yr_target[yr_i]) , 
                                            sat_target_yr, 
                                            by='sta_code')
                              }
         ) %>% do.call(rbind, .)
         no2_e_sub <- no2_e_sub[!is.na(no2_e_sub$sat_pm25),]
      }
      # Add met
      
      met_target <- met[, met_yr%in%yr_target]
      met_var <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist %>% unique
      met_str <- lapply(met_var, function(met_str) paste0(met_str, "_", no2_e_sub$year))
      no2_e_sub_s <- inner_join(no2_e_sub, met, "sta_code")
      met_target <- sapply(seq_along(met_str), 
                           function(str_i){
                              sapply(seq_along(met_str[[str_i]]), function(i) no2_e_sub_s[i, met_str[[str_i]][i] ])
                           }
      ) %>% as.data.frame()
      names(met_target) <- met_var
      # identical(no2_e_sub_s$sta_code, no2_e_sub$sta_code)
      no2_e_sub <- cbind(no2_e_sub, met_target)
   }
   no2_e_sub %>% mutate(cntr_code=factor(cntr_code), year=factor(year),
                        zoneID=factor(zoneID))
}
