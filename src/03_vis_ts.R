library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
library(sf)
library(data.table)
library(tmap)
library(rlang)
library(raster)
target_polls <- c('NO2', 'PM25', 'PM10', 'O3')
target_polls2 <- c('NO2', 'PM2.5', 'PM10', 'O3')

#-------decreasing trends----


obs_mean <- lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, climate_zone, component_caption) %>% 
      summarise(obs=mean(obs),
                `gtwr00-19`=mean(`gtwr_00-19`),
                n=n()
                # gwr=mean(gwr_singleyear),
                # `slr00-19`=mean(`slr_00-19`),
                # slr=mean(slr_singleyear)
      )
   obs2 %>% gather('model', 'values', -c('year', 'climate_zone', 'component_caption','n'))
}) %>% do.call(rbind, .)
lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, component_caption) %>% 
      summarise(obs=round(mean(obs), 3),
                `gtwr00-19`=round(mean(`gtwr_00-19`), 3),
                n=n()
                # gwr=mean(gwr_singleyear),
                # `slr00-19`=mean(`slr_00-19`),
                # slr=mean(slr_singleyear)
      )
}) %>% do.call(rbind, .) %>% 
   write.csv(., 'results/output/obs_stat.csv', row.names = F)

obs_q10 <- lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, climate_zone, component_caption) %>% 
      summarise(obs=quantile(obs, probs=0.1),
                `gtwr00-19`=quantile(`gtwr_00-19`, probs=0.1),
                n=n()
      )
   obs2 %>% gather('model', 'values', -c('year', 'climate_zone', 'component_caption','n'))
}) %>% do.call(rbind, .)
obs_q90 <- lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, climate_zone, component_caption) %>% 
      summarise(obs=quantile(obs, probs=0.9),
                `gtwr00-19`=quantile(`gtwr_00-19`, probs=0.9),
                n=n()
      )
   obs2 %>% gather('model', 'values', -c('year', 'climate_zone', 'component_caption','n'))
}) %>% do.call(rbind, .)
obs_q50 <- lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, climate_zone, component_caption) %>% 
      summarise(obs=quantile(obs, probs=0.5),
                `gtwr00-19`=quantile(`gtwr_00-19`, probs=0.5),
                n=n()
      )
   obs2 %>% gather('model', 'values', -c('year', 'climate_zone', 'component_caption','n'))
}) %>% do.call(rbind, .)
obs_mean <- obs_mean %>% mutate(values=ifelse(component_caption=='PM2.5'&year<2006&(model%in%c('slr','gwr')),
                                          NA,
                                          values)) %>% 
   mutate(model=factor(model, levels=c('obs', 'gtwr00-19')))
obs_q10 <- obs_q10 %>% mutate(values=ifelse(component_caption=='PM2.5'&year<2006&(model%in%c('slr','gwr')),
                                              NA,
                                              values)) %>% 
   mutate(model=factor(model, levels=c('obs', 'gtwr00-19')))
obs_q90 <- obs_q90 %>% mutate(values=ifelse(component_caption=='PM2.5'&year<2006&(model%in%c('slr','gwr')),
                                            NA,
                                            values)) %>% 
   mutate(model=factor(model, levels=c('obs', 'gtwr00-19')))
obs_q50 <- obs_q50 %>% mutate(values=ifelse(component_caption=='PM2.5'&year<2006&(model%in%c('slr','gwr')),
                                            NA,
                                            values)) %>% 
   mutate(model=factor(model, levels=c('obs', 'gtwr00-19')))
ggplot()+
   geom_line(data=obs_mean, aes(x=year, y=values, col=model), alpha=0.8)+
   geom_line(data=obs_q10, aes(x=year, y=values, col=model), lty=2)+
   geom_line(data=obs_q90, aes(x=year, y=values, col=model), lty=2)+
   # geom_line(data=obs_q50, aes(x=year, y=values, col=model), lty=2)+
   theme_bw()+
   scale_color_manual(values=c('black','red'))+
   labs(color='',y=expression(concentrations~(ug/m^3)))+
   facet_wrap(component_caption~climate_zone, scales='free')
ggsave('graph/obs_ts.tiff', width = 10, height=6)

# --------archived: all linear models -----
obs_all <- lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, climate_zone, component_caption) %>% 
      summarise(obs=mean(obs),
                `gtwr00-19`=mean(`gtwr_00-19`),
                gwr=mean(gwr_singleyear),
                `slr00-19`=mean(`slr_00-19`),
                slr=mean(slr_singleyear)
                )
   obs2 %>% gather('model', 'values', -c('year', 'climate_zone', 'component_caption'))
}) %>% do.call(rbind, .)
obs_sd <- lapply(1:4, function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   no2 <- fread(paste0('data/processed/gee/predictionsAll_obs_finalModel_', target_poll, '.csv')) %>% as.data.frame()
   # no2$`system:index` <- unlist(lapply(strsplit(no2$`system:index`, '_'), `[[`, 2))
   no2 <- no2 %>% dplyr::select(-'system:index', -'.geo')
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   obs <- read_data(target_poll2, 2000:2019)
   obs$zoneID <- as.numeric(obs$zoneID)
   obs <- inner_join(
      obs, read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
      by='zoneID')
   obs <- obs[!duplicated(obs$sta_code),]
   obs <- obs %>% dplyr::select(climate_zone, sta_code, zoneID)
   obs_combined <- inner_join(no2, obs, by='sta_code')
   obs2 <- obs_combined %>% 
      group_by(year, climate_zone, component_caption) %>% 
      summarise(obs=sd(obs),
                `gtwr00-19`=sd(`gtwr_00-19`),
                gwr=sd(gwr_singleyear),
                `slr00-19`=sd(`slr_00-19`),
                slr=sd(slr_singleyear)
      )
   obs2 %>% gather('model', 'values', -c('year', 'climate_zone', 'component_caption'))
}) %>% do.call(rbind, .)
obs_sd <- obs_sd %>% mutate(values=ifelse(component_caption=='PM2.5'&year<2006&(model%in%c('slr','gwr')),
                                     NA,
                                     values)) %>% 
   mutate(model=factor(model, levels=c('obs', 'slr', 'slr00-19',
                                       'gwr', 'gtwr00-19')))
obs_all <- obs_all %>% mutate(values=ifelse(component_caption=='PM2.5'&year<2006&(model%in%c('slr','gwr')),
                                     NA,
                                     values)) %>% 
   mutate(model=factor(model, levels=c('obs', 'slr', 'slr00-19',
                                       'gwr', 'gtwr00-19')))

ggplot(obs_all)+
   geom_line(aes(x=year, y=values, col=model), alpha=0.8)+
   theme_bw()+
   scale_color_manual(values=c('black','cyan','blue','pink','red'))+
   facet_wrap(component_caption~climate_zone, scales='free')

ggplot(obs_sd)+
   geom_line(aes(x=year, y=values, col=model), alpha=0.8)+
   theme_bw()+
   scale_color_manual(values=c('black','cyan','blue','pink','red'))+
   facet_wrap(component_caption~climate_zone, scales='free')

