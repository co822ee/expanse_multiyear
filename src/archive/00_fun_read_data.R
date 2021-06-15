# Read data for the whole EXPANSE area
# For multiple-year modelling where different predictor values are used
library(dplyr)
library(raster)
seed <- 123
local_crs <- CRS("+init=EPSG:3035")
target_poll = 'NO2'
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
# Read in station metadata
# sta <- read.csv("../EXPANSE_predictor/data/processed/airbase_station_climate.csv")
sta <- read.csv("../EXPANSE_algorithm/data/processed/airbase_station_climate_gwrGrd.csv")
# There are 3 missing rows and 7 rows with unknown values for sta_type
# Remove them
sta <- sta %>% filter(sta_type!="Unknown", sta_type!="")
# Read in airbase data
# airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_checked.csv")
airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_pollutants4_day_hr.csv")
no2 <- airbase %>% filter(component_caption==target_poll)
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value, sta_code=station_european_code)
# str(no2)
# str(sta)
no2_sta <- inner_join(no2 %>% dplyr::select(-country_code), sta %>% dplyr::select("sta_code", "zoneID", "id_gwr"), by="sta_code") 
# the amount of data decreases because some stations with unkown station types are removed

# Read in predictor values
road <- read.csv("../EXPANSE_predictor/data/processed/road_merged.csv")

macc <- read.csv("../EXPANSE_predictor/data/raw/gee/maccAndPop.csv")
macc <- macc %>% rename(sta_code=station_european_code) %>% 
   dplyr::select(sta_code, pop, macc)

pred <- inner_join(road, macc, by="sta_code")

no2_e_all <- inner_join(no2_sta, pred, by="sta_code")
rm(road, macc, pred, sta, airbase, no2, no2_sta)

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
names(omi)
## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   # Read in linear interpolated lc values
   lc_filenames <- list.files("../EXPANSE_predictor/data/processed/linear_inter_lc/", "pred")
   lc_names <- gsub(".csv", "", lc_filenames)
   
   # Single year
   if(length(yr_target)==1){
      # lc
      if(all(paste0("pred_", yr_target)%in%lc_names)){
         lc <- read.csv(paste0("../EXPANSE_predictor/data/processed/linear_inter_lc/pred_", yr_target,".csv"))
         no2_e_sub <- inner_join(no2_e_sub, lc[, c('sta_code', names(lc)[!names(lc)%in%names(no2_e_sub)])], 
                                 by='sta_code')
         
      }
      # Add omi (if the year include omi)
      if(all(paste0("omi_", yr_target)%in%names(omi))){
         omi_target <- omi[, c("sta_code",paste0("omi_",yr_target))] %>% 
            rename(omi=paste0("omi_",yr_target))
         no2_e_sub <- inner_join(no2_e_sub, omi_target, by="sta_code")
      }
      # Add met
      if(all(yr_target%in%met_yr)){
         met_target <- met[, met_yr==yr_target]
         # names(met_target) <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist()
         met_target$sta_code <- met$sta_code
         no2_e_sub <- inner_join(no2_e_sub, met_target, by="sta_code")
      }
      
   }else{
      # Multiple years
      # lc
      if(all(paste0("pred_", yr_target)%in%lc_names)){
         target_yrs <- no2_e_sub$year
         lc_list <- lapply(yr_target, function(yr) read.csv(paste0("../EXPANSE_predictor/data/processed/linear_inter_lc/pred_", yr,".csv")))
         names(lc_list) <- paste0("pred_", yr_target)
         lc_str <- paste0("pred_", target_yrs)
         lc_target <- lapply(seq_along(lc_str), 
                              function(row_i){
                                 lc_target_yr <- lc_list[[lc_str[row_i]]]
                                 lc_target_yr[lc_target_yr$sta_code==no2_e_sub$sta_code[row_i], ]
                              }
         ) %>% do.call(rbind, .)
         
         no2_e_sub <- cbind(no2_e_sub, lc_target[,!names(lc_target)%in%names(no2_e_sub)])
         
      }
      # Add omi
      if(all(paste0("omi_", yr_target)%in%names(omi))){
         omi_target <- omi[, paste0("omi_",yr_target)]
         omi_var <- lapply(strsplit(names(omi_target), "_"), `[[`, 1) %>% unlist %>% unique
         omi_str <- lapply(omi_var, function(omi_str) paste0(omi_str, "_", no2_e_sub$year))
         no2_e_sub_s <- inner_join(no2_e_sub, omi, "sta_code")
         omi_target <- sapply(seq_along(omi_str), 
                              function(str_i){
                                 sapply(seq_along(omi_str[[str_i]]), function(i) no2_e_sub_s[i, omi_str[[str_i]][i] ])
                              }
         ) %>% as.data.frame()
         names(omi_target) <- omi_var
         # identical(no2_e_sub_s$sta_code, no2_e_sub$sta_code)
         no2_e_sub <- cbind(no2_e_sub, omi_target)
         
      }
      # Add met
      if(all(yr_target%in%met_yr)){
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
      
   }
   no2_e_sub %>% mutate(cntr_code=factor(cntr_code), year=factor(year),
                        zoneID=factor(zoneID), id_gwr=factor(id_gwr))
}
