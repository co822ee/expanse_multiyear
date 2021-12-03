library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
library(data.table)
library(sf)
library(APMtools)
library(gridExtra)
source("scr/fun_call_lib.R")
target_polls <- c('NO2', 'PM10', 'PM25')
target_polls2 <- c('NO2', 'PM10', 'PM2.5')
escape_obss <- c('no2', 'pm10', 'pm25')
R2s <- c(0.494, 0, 0.648)
RMSEs <- c(11.47, 0, 3.41)
##### Useful functions
read_perfm <- function(poll){
   if(poll=='PM2.5'){
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower('pm25'),'All.csv'))
   }else{
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower(poll),'All.csv'))
   }
   
   predictor <- predictor[!duplicated(predictor$sta_code), ] %>% 
      dplyr::select(zoneID, sta_code) %>% 
      inner_join(
         read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
         .,
         by='zoneID')
   
   csv_names <- gsub('SLR_result_all_', '', 
                     list.files('../EXPANSE_algorithm/data/workingData/', 
                                paste0('SLR_result_all_o3_', poll))) %>% 
      strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   # years <- csv_names %>% substr(., nchar(csv_names)-3, nchar(csv_names)) %>% 
   #    as.numeric() %>% as.list()
   all_test <- lapply(paste0("../EXPANSE_algorithm/data/workingData/5cv_", csv_names, ".csv"), read.csv)
   
   all_test <- lapply(all_test, function(df_all){
      df_all$poll <- poll
      df_all$period <- df_all$year
      df_all %>% inner_join(., predictor, by='sta_code')
   })
   all_test
}
read_perfm2 <- function(poll){
   if(poll=='PM2.5'){
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower('pm25'),'All.csv'))
   }else{
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower(poll),'All.csv'))
   }
   predictor <- predictor[!duplicated(predictor$sta_code), ] %>% 
      dplyr::select(zoneID, sta_code) %>% 
      inner_join(
         read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
         .,
         by='zoneID')
   
   csv_names2 <- gsub('SLR_result_all_', '', 
                      list.files('../expanse_multiyear/data/workingData/', 
                                 paste0('SLR_result_all_o3_', poll))) %>% 
      strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   # # nfold=4 PM2.5 from year 2000-2003 (00-19) there is NA
   all_test2 <- lapply(paste0("../expanse_multiyear/data/workingData/5cv_", csv_names2, ".csv"), 
                       read.csv)
   
   all_test2 <- lapply(seq_along(all_test2), function(i){
      df_all <- all_test2[[i]]
      df_all$poll <- poll
      df_all$period <- strsplit(csv_names2, '_')[[i]][3]
      df_all %>% inner_join(., predictor, by='sta_code')
      # if(poll=='PM2.5'){
      #    # df_all[!is.na(df_all$gtwr), ]
      #    # We just leave out the gtwr 5-CV for 2000-2003 because the GTWR for fold4 for 2000-2003 could not be built
      #    # Also the numbers of observations for 2000-2003 are 11,22,52,82...
      #    df_all
      # }else{
      #    df_all
      # }
   })
   
   all_test2
}
show_EM <- function(all_df_i, all_test, climate_included=F, fold_included=F){
   
   df_all <- lapply(sort(unique(all_test[[all_df_i]]$year)), 
                    function(target_yr){
                       all_test_sub <- all_test[[all_df_i]][all_test[[all_df_i]]$year==target_yr,]
                       slr=all_test_sub$slr
                       gwr=all_test_sub$gwr
                       gtwr=all_test_sub$gtwr
                       
                       rf=all_test_sub$rf
                       obs=all_test_sub$obs
                       df_all <- data.frame(slr=error_matrix(obs, slr),
                                            gwr=error_matrix(obs, gwr),
                                            gtwr=error_matrix(obs, gtwr),
                                            rf=error_matrix(obs, rf),
                                            period=unique(all_test[[all_df_i]]$period),
                                            year=target_yr,
                                            poll=unique(all_test[[all_df_i]]$poll))[c(1,5,7),]
                       
                       df_all$EM = row.names(df_all)
                       if(climate_included){
                          df_all$climate_zone <- unique(all_test_sub$climate_zone)
                          df_all$climate_name <- paste0(df_all$climate_zone, '_', nrow(all_test_sub))
                       }
                       if(fold_included){
                          df_all$data <- unique(all_test_sub$data)
                       }
                       df_all
                    }) %>% do.call(rbind, .)
   
   
}


outputCleanFiles <- function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   
   files <- list.files('data/processed/gee/', 'predictionsAll_escape')
   files <- files[grepl(target_poll, files)]
   files2 <- list.files('data/workingData/', 'RF_result_validation_escape_')
   files2 <- files2[grepl(target_poll2, files2)]
   files3 <- list.files('../EXPANSE_algorithm/data/workingData/', 'RF_result_validation_escape_')
   files3 <- files3[grepl(target_poll2, files3)]
   
   no2_l <- lapply(paste0('data/processed/gee/', files[!grepl('RF', files)]), 
                   read.csv)  #
   no2_linear <- no2_l[[1]]
   # Multiple-year modelling
   no2_rf_pure_l <- lapply(paste0('data/workingData/', files2), 
                           read.csv)  #add a index
   model_names <- paste0('rf_', gsub('.csv', '', unlist(lapply(strsplit(files2, '_'), `[[`, 6))) %>% 
                            gsub('-', '.', .),
                         '_2010')
   no2_rf_pure_l <- lapply(seq_along(no2_rf_pure_l), function(i){
      return(no2_rf_pure_l[[i]] %>% rename(setNames('rf', model_names[i])))
   })
   
   # Single-year modelling
   no2_rf_pure_l3 <- lapply(paste0('../EXPANSE_algorithm/data//workingData/', files3), 
                            read.csv)  #add a index
   model_names3 <- paste0('rf_', gsub('.csv', '', unlist(lapply(strsplit(files3, '_'), `[[`, 6))) %>% 
                             gsub('-', '.', .),
                          '_2010')
   no2_rf_pure_l3 <- lapply(seq_along(no2_rf_pure_l3), function(i){
      return(no2_rf_pure_l3[[i]] %>% rename(setNames('rf', model_names3[i])))
   })
   
   
   no2_rf_pure <- Reduce(
      function(x, y, ...) inner_join(x, y,  by=c('allid'), ...), 
      no2_rf_pure_l%>% 
         lapply(., function(df_all) df_all %>% dplyr::select(allid, matches('rf_')))
   )
   
   no2_rf_pure3 <- Reduce(
      function(x, y, ...) inner_join(x, y,  by=c('allid'), ...), 
      no2_rf_pure_l3%>% 
         lapply(., function(df_all) df_all %>% dplyr::select(allid, matches('rf_')))
   )
   
   
   no2 <- inner_join(no2_linear, no2_rf_pure, by=names(no2_linear)[names(no2_linear)%in%names(no2_rf_pure)])
   no2 <- inner_join(no2, no2_rf_pure3, by=names(no2)[names(no2)%in%names(no2_rf_pure3)])
   write.csv(no2, paste0('data/processed/prediction_escape_fromR_', target_poll2), row.names=F)
}

outputEscapeR2 <- function(poll_i){
   # ESCAPE data
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   no2 <- read.csv(paste0('data/processed/prediction_escape_fromR_', target_poll2))
   
   no2_clean <- no2[, grepl('slr|gwr|rf|gtwr', names(no2))]
   no2_clean <- no2_clean[, !grepl('no2lurfull', names(no2_clean))]
   model_years <- lapply(strsplit(names(no2_clean), "_"), `[[`, 3) %>% unlist
   # Select predictions for the year 2010
   no2_clean <- no2_clean[, model_years=='2010']
   models_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 1) %>% unlist
   # substr(names(no2_clean), nchar(names(no2_clean))-9, nchar(names(no2_clean))-5) 
   
   no2_clean <- no2_clean[, order(models_name)]
   scenarios_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 2) %>% unlist
   
   # Divide by country
   if(target_poll=='NO2'){
      raw_df <- read.csv('data/raw/NO2_ESCAPE_2010_Feb2018_updated_for_SGR_without_SPB.csv')
   }else{
      raw_df <- read.csv('data/raw/PM25_ESCAPE_2010_Feb2018_excluding_SGR.csv')
   }
   no2 <- inner_join(no2, raw_df %>% dplyr::select(allid, x_etrs, y_etrs), by='allid')
   eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
   no2_p <- st_as_sf(no2, coords = c('x_etrs', 'y_etrs'), crs=st_crs(eu_bnd))
   join_t <- st_join(eu_bnd, no2_p)
   join_t <- join_t[!is.na(join_t$allid),]
   no2 <- as.data.frame(join_t)
   
   library(APMtools)
   em_df <- lapply(names(no2_clean), function(pred_name){
      scenario_name <- substr(pred_name, 1, nchar(pred_name)-5)
      model_name <- unlist(strsplit(pred_name, '_'))[1]
      error_matrix(no2[, escape_obs], no2[, pred_name]) %>% 
         t() %>% 
         as.data.frame() %>% 
         mutate(model=model_name, scenario=scenario_name)
   }) %>% do.call(rbind, . )
   em_df$scenario <- gsub('\\.', '-', em_df$scenario)
   em_df$period <- lapply(strsplit(em_df$scenario, '_'), `[[`, 2) %>% unlist
   em_df$poll <- target_poll2
   em_df$year <- 2010
   dat_r2_escape <- em_df %>% dplyr::select(rsq, RMSE, model, scenario, poll, period, year) %>%  
      gather("EM", "values", c(rsq, RMSE)) %>% 
      dplyr::select(-scenario)
   dat_r2_escape <- dat_r2_escape[, order(names(dat_r2_escape))]
   dat_r2_escape$data <- 'escape (LUR)'
   dat_r2_escape
}
plotEscapeRMSE <- function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   no2 <- read.csv(paste0('data/processed/prediction_escape_fromR_', target_poll2))
   
   no2_clean <- no2[, grepl('slr|gwr|rf|gtwr', names(no2))]
   no2_clean <- no2_clean[, !grepl('no2lurfull', names(no2_clean))]
   model_years <- lapply(strsplit(names(no2_clean), "_"), `[[`, 3) %>% unlist
   # Select predictions for the year 2010
   no2_clean <- no2_clean[, model_years=='2010']
   models_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 1) %>% unlist
   # substr(names(no2_clean), nchar(names(no2_clean))-9, nchar(names(no2_clean))-5) 
   
   no2_clean <- no2_clean[, order(models_name)]
   scenarios_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 2) %>% unlist
   # no2_clean <- no2_clean[, order(scenarios_name)]
   # no2_clean <- no2_clean[, mixedsort(names(no2_clean))]
   
   
   if(target_poll=='NO2'){
      range_limit <- c(0, 140)
   }else{
      range_limit <- c(0, 45)
      ## Exclude 1st-version GTWR (remove this line after 2nd-version GTWR is done)
      no2_clean <- no2_clean[, !grepl('gtwr', names(no2_clean))]
   }
   # Divide by country
   if(target_poll=='NO2'){
      raw_df <- read.csv('data/raw/NO2_ESCAPE_2010_Feb2018_updated_for_SGR_without_SPB.csv')
   }else{
      raw_df <- read.csv('data/raw/PM25_ESCAPE_2010_Feb2018_excluding_SGR.csv')
   }
   no2 <- inner_join(no2, raw_df %>% dplyr::select(allid, x_etrs, y_etrs), by='allid')
   eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
   no2_p <- st_as_sf(no2, coords = c('x_etrs', 'y_etrs'), crs=st_crs(eu_bnd))
   join_t <- st_join(eu_bnd, no2_p)
   join_t <- join_t[!is.na(join_t$allid),]
   no2 <- as.data.frame(join_t)
   em_df <- lapply(names(no2_clean), function(pred_name){
      scenario_name <- substr(pred_name, 1, nchar(pred_name)-5)
      model_name <- unlist(strsplit(pred_name, '_'))[1]
      no2_df <- lapply(unique(no2$CNTR_ID), function(cntrid){
         no2_df <- no2 %>% filter(CNTR_ID==cntrid)
         error_matrix(no2_df[, escape_obs], no2_df[, pred_name]) %>%
            t() %>%
            as.data.frame() %>%
            mutate(model=model_name, scenario=scenario_name, CNTR_ID=cntrid,
                   NAME_ENGL=unique(no2_df$NAME_ENGL))
         
      }) %>% do.call(rbind,. )
      no2_df
   }) %>% do.call(rbind, . )
   library(APMtools)
   em_df <- lapply(names(no2_clean), function(pred_name){
      scenario_name <- substr(pred_name, 1, nchar(pred_name)-5)
      model_name <- unlist(strsplit(pred_name, '_'))[1]
      error_matrix(no2[, escape_obs], no2[, pred_name]) %>% 
         t() %>% 
         as.data.frame() %>% 
         mutate(model=model_name, scenario=scenario_name)
   }) %>% do.call(rbind, . )
   if(target_poll=='NO2'){
      
      p2 <- ggplot(em_df %>% dplyr::select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')),
                   aes(x=scenario, y=values, fill=model))+  #fill=model
         geom_bar(stat="identity", position = "dodge2")+
         # facet_grid(EM~., scales ='free')+
         labs(y='RMSE', title = target_poll)+
         coord_flip()+
         theme(axis.title = element_text(size = 13),
               axis.text = element_text(size = 11),
               legend.title = element_text(size = 13),
               legend.text = element_text(size = 13),
               strip.text.y = element_text(size = 12))+
         geom_hline(aes(yintercept=elapseRMSE))
         # lims(y=c(0,13))
   }else{
      p2 <- ggplot(em_df %>% dplyr::select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')),
                   aes(x=scenario, y=values, fill=model))+  #fill=model
         geom_bar(stat="identity", position = "dodge2")+
         # facet_grid(EM~., scales ='free')+
         labs(y='RMSE', title = target_poll)+
         coord_flip()+
         theme(axis.title = element_text(size = 13),
               axis.text = element_text(size = 11),
               legend.title = element_text(size = 13),
               legend.text = element_text(size = 13),
               strip.text.y = element_text(size = 12))
   }
   p2+guides(fill = guide_legend(reverse = TRUE))
}

plotESCAPEMaps <- function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   no2 <- read.csv(paste0('data/processed/prediction_escape_fromR_', target_poll2))
   
   # Divide by country
   if(target_poll=='NO2'){
      raw_df <- read.csv('data/raw/NO2_ESCAPE_2010_Feb2018_updated_for_SGR_without_SPB.csv')
   }else{
      raw_df <- read.csv('data/raw/PM25_ESCAPE_2010_Feb2018_excluding_SGR.csv')
   }
   no2 <- inner_join(no2, raw_df %>% dplyr::select(allid, x_etrs, y_etrs), by='allid')
   eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
   no2_p <- st_as_sf(no2, coords = c('x_etrs', 'y_etrs'), crs=st_crs(eu_bnd))
   tm_shape(eu_bnd)+
      tm_borders()+
      tm_shape(no2_p)+
      tm_dots()+
      tm_layout(title=paste0('ESCAPE ', target_poll2),
                frame = T)
}


tmapsList <- lapply(seq_along(target_polls), plotESCAPEMaps)
# tmaps <- do.call(tmap_arrange, tmapsList)
# tmap_save(tmaps, paste0('graph/ESCAPE_maps.tiff'),
#           dpi=150, height=4, width=14, units='in')  
#--------- Main ----------
# Clean files
lapply(seq_along(target_polls), outputCleanFiles)
# Number of points
lapply(seq_along(target_polls), function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   no2 <- read.csv(paste0('data/processed/prediction_escape_fromR_', target_poll2))
   nrow(no2)
   
})
ps_l <- lapply(seq_along(target_polls), plotEscapeR2)
ps_lrmse <- lapply(seq_along(target_polls), plotEscapeRMSE)

# plot R2

## Read in 5-fold CV results
calc_perfm <- function(all_test){
   em_df_l <- lapply(seq_along(all_test), show_EM, all_test=all_test)
   do.call(rbind, em_df_l)
}
all_test_l <- lapply(c(target_polls2, 'O3'), read_perfm)
em_df5fold <- lapply(all_test_l, calc_perfm)
em_df5fold <- do.call(rbind, em_df5fold)

all_test_l2 <- lapply(c(target_polls2, 'O3'), read_perfm2)
em_df5fold2 <- lapply(all_test_l2, calc_perfm)
em_df5fold2 <- do.call(rbind, em_df5fold2)


em_df_all <- rbind(em_df5fold, em_df5fold2)
dat_r2 <- em_df_all %>% filter(EM=='rsq', !period%in%c('08-10', '09-11', '10-12', 
                                                       '08-12', '06-12', '12-19'),
                               year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period'))
dat_r2 <- dat_r2[!is.na(dat_r2$values), ] %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))
dat_r2 <- dat_r2[, order(names(dat_r2))]
dat_r2$data <- '5-fold CV (LUR)'
## REad in 5-fold back-extrapolation (using gtwr00-19 for 2010 as the benchmark surface)
em_df_back <- lapply(c(target_polls, 'O3'), function(target_pollss){
   target_poll <- target_pollss
   if(target_pollss=='PM25'){
      target_poll2 <- 'PM2.5'
   }else{
      target_poll2 <- target_pollss
   }
   back_extrapo <- fread(paste0('../expanse_multiyear/data/processed/gee/predictionsAll_5fold_', target_poll, '.csv'))
   back_extrapo <- back_extrapo %>% dplyr::select(-'system:index', -'.geo')
   scenario_names <- names(back_extrapo)[!names(back_extrapo)%in%c('component_caption', 'obs', 'year', 'sta_code', 'sta_type')]
   back_extrapo_new <- lapply(scenario_names, function(scenario_name){
      new_name <- strsplit(scenario_name, '_')[[1]][1]
      
      out <- back_extrapo %>% dplyr::select('year', 'sta_code', scenario_name) %>% 
         rename(setNames(scenario_name, new_name))
      out$data <- lapply(strsplit(scenario_name, '_'), `[[`, 3)
      out$nfold <- strsplit(scenario_name, '_')[[1]][2] %>% gsub('fold', '', .) %>% as.numeric
      out
   }) %>% do.call(rbind,.)
   obs <- read.csv(paste0("../expanse_multiyear/data/workingData/5cv_o3_", target_poll2, '_00-19.csv')) %>% 
      dplyr::select(obs, sta_code, year, nfold)
   back_extrapo_all <- left_join(obs, back_extrapo_new, by=c('sta_code', 'year', 'nfold'))
   back_extrapo_all <- back_extrapo_all[!duplicated(back_extrapo_all),]
   fold_data_i <- expand.grid(nfold=unique(back_extrapo_all$nfold), 
                              data=as.character(unique(back_extrapo_all$data)))
   back_extrapo_all_l <- lapply(as.character(unique(back_extrapo_all$data)), function(data_i){
      back_extrapo_all %>% filter(data==data_i) %>% 
         mutate(period='00-19', poll=target_poll2)
   })
   lapply(seq_along(back_extrapo_all_l), show_EM, all_test=back_extrapo_all_l, fold_included=T) %>% 
      do.call(rbind, .)
}) %>% 
   do.call(rbind, .)
dat_r2_back <- em_df_back %>%
   dplyr::select(-slr, -gwr, -rf) %>% 
   filter(EM=='rsq', !period%in%c('08-10', '09-11', '10-12', 
                                  '08-12', '06-12', '12-19'),
          year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period', 'data'))
dat_r2_back <- dat_r2_back[!is.na(dat_r2_back$values), ] %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))
dat_r2_back <- dat_r2_back[, order(names(dat_r2_back))]
dat_r2_back$data <- paste0('5-fold CV ', '(', dat_r2_back$data, ')')

## Read in ESCAPE results
dat_r2_escape <- lapply(seq_along(target_polls2), outputEscapeR2) %>% 
   do.call(rbind,.) %>% 
   filter(EM=='rsq', !period%in%c('08-10', '09-11', '10-12', 
                                  '08-12', '06-12', '12-19'),
          year%in%c(2000, 2005, 2010, 2015, 2019)) %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))


## 
dat_r2_all <- rbind(dat_r2, dat_r2_back %>% filter(year!=2010), dat_r2_escape)
dat_r2_all <- dat_r2_all %>%
   filter(model!='rf'&model!='rf00-19') %>% 
   mutate(model=factor(model, levels = c('gtwr00-19', 'gwr', 'slr00-19', 'slr')),
          data=factor(data, levels = unique(dat_r2_all$data)[c(3,2,1,4)]))

ggplot(dat_r2_all, 
       aes(x=reorder(model, as.numeric(model)), y=values, 
           shape=data, color=data))+
   geom_point(size=2, alpha=0.8)+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="R squared", x='')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   facet_grid(year~poll)+
   coord_flip()+
   theme_bw()+
   guides(fill = guide_legend(reverse = TRUE))
ggsave('graph/R2_v2excludeRF.tiff', width=7.5, height=4, units='in', dpi=200)

### RMSE
dat_r2 <- em_df_all %>% filter(EM=='RMSE', !period%in%c('08-10', '09-11', '10-12', 
                                                       '08-12', '06-12', '12-19'),
                               year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period'))
dat_r2 <- dat_r2[!is.na(dat_r2$values), ] %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))
dat_r2 <- dat_r2[, order(names(dat_r2))]
dat_r2$data <- '5-fold CV (LUR)'

dat_r2_back <- em_df_back %>%
   dplyr::select(-slr, -gwr, -rf) %>% 
   filter(EM=='RMSE', !period%in%c('08-10', '09-11', '10-12', 
                                  '08-12', '06-12', '12-19'),
          year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period', 'data'))
dat_r2_back <- dat_r2_back[!is.na(dat_r2_back$values), ] %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))
dat_r2_back <- dat_r2_back[, order(names(dat_r2_back))]
dat_r2_back$data <- paste0('5-fold CV ', '(', dat_r2_back$data, ')')

## Read in ESCAPE results
dat_r2_escape <- lapply(seq_along(target_polls2), outputEscapeR2) %>% 
   do.call(rbind,.) %>% 
   filter(EM=='RMSE', !period%in%c('08-10', '09-11', '10-12', 
                                  '08-12', '06-12', '12-19'),
          year%in%c(2000, 2005, 2010, 2015, 2019)) %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))


## 
dat_r2_all <- rbind(dat_r2, dat_r2_back %>% filter(year!=2010), dat_r2_escape)
dat_r2_all <- dat_r2_all %>%
   filter(model!='rf'&model!='rf00-19') %>% 
   mutate(model=factor(model, levels = c('gtwr00-19', 'gwr', 'slr00-19', 'slr')),
          data=factor(data, levels = unique(dat_r2_all$data)[c(3,2,1,4)]))

ggplot(dat_r2_all, 
       aes(x=reorder(model, as.numeric(model)), y=values, 
           shape=data, color=data))+
   geom_point(size=2, alpha=0.8)+
   labs(y="RMSE", x='')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   facet_grid(year~poll, scales = 'free_x')+
   coord_flip()+
   theme_bw()+
   guides(fill = guide_legend(reverse = TRUE))
ggsave('graph/RMSE_v2excludeRF.tiff', width=7.5, height=4, units='in', dpi=200)

