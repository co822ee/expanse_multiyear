library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
library(sf)
target_poll <- 'NO2'    # PM25
target_poll2 <- 'NO2'   # PM2.5
escape_obs <- 'no2'
elapseR2 <- 0.494          ## 0.494 for NO2 and 0.648 for pm25
elapseRMSE <- 11.47          ### 11.47 for no2 3.41 for pm25
# files <- paste0('data/processed/gee/predictionsAll_escape', target_poll,'.csv')
target_polls <- c('NO2', 'PM25', 'PM10')
target_polls2 <- c('NO2', 'PM2.5', 'PM10')
escape_obss <- c('no2', 'pm25', 'pm10')
R2s <- c(0.494, 0.648, 0)
RMSEs <- c(11.47, 3.41, 0)

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
plotEscapeR2 <- function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   no2 <- read.csv(paste0('data/processed/prediction_escape_fromR_', target_poll2))
   
   no2_clean <- no2[, grepl('slr|gwr|rf', names(no2))]
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
      p1 <- ggplot(em_df %>% dplyr::select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
                   aes(x=scenario, y=values, fill=model))+  #fill=model
         geom_bar(stat="identity", position = "dodge2")+
         # facet_grid(EM~., scales ='free')+
         labs(y='R2', title = target_poll)+
         coord_flip()+
         theme(axis.title = element_text(size = 13),
               axis.text = element_text(size = 11),
               legend.title = element_text(size = 13),
               legend.text = element_text(size = 13),
               strip.text.y = element_text(size = 12))+
         geom_hline(aes(yintercept=elapseR2))+
         lims(y=c(0, 0.6))
   }else if(target_poll=='PM25'){
      p1 <- ggplot(em_df %>% dplyr::select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
                   aes(x=scenario, y=values, fill=model))+  #fill=model
         geom_bar(stat="identity", position = "dodge2")+
         # facet_grid(EM~., scales ='free')+
         labs(y='R2', title = target_poll)+
         coord_flip()+
         theme(axis.title = element_text(size = 13),
               axis.text = element_text(size = 11),
               legend.title = element_text(size = 13),
               legend.text = element_text(size = 13),
               strip.text.y = element_text(size = 12))+
         geom_hline(aes(yintercept=elapseR2))
      
   }else{
      p1 <- ggplot(em_df %>% dplyr::select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
                   aes(x=scenario, y=values, fill=model))+  #fill=model
         geom_bar(stat="identity", position = "dodge2")+
         # facet_grid(EM~., scales ='free')+
         labs(y='R2', title = target_poll)+
         coord_flip()+
         theme(axis.title = element_text(size = 13),
               axis.text = element_text(size = 11),
               legend.title = element_text(size = 13),
               legend.text = element_text(size = 13),
               strip.text.y = element_text(size = 12))
   }
   
   p1
}
plotEscapeRMSE <- function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   escape_obs <- escape_obss[poll_i]  #'no2'
   elapseR2 <- R2s[poll_i]     ## 0.494 for NO2 and 0.648 for pm25
   elapseRMSE <- RMSEs[poll_i]          ### 11.47 for no2 3.41 for pm25
   no2 <- read.csv(paste0('data/processed/prediction_escape_fromR_', target_poll2))
   
   no2_clean <- no2[, grepl('slr|gwr|rf', names(no2))]
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
         geom_hline(aes(yintercept=elapseRMSE))+
         lims(y=c(0,13))
   }else if(target_poll=='PM25'){
      
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
}

lapply(seq_along(target_polls), outputCleanFiles)

ps_l <- lapply(seq_along(target_polls), plotEscapeR2)
ps_lrmse <- lapply(seq_along(target_polls), plotEscapeRMSE)

png('graph/escape_r2.png', 
    width = 8, height = 10, units='in', res=200)
do.call(grid.arrange, ps_l)
dev.off()

png('graph/escape_rmse.png', 
    width = 8, height = 10, units='in', res=200)
do.call(grid.arrange, ps_lrmse)
dev.off()
# ggplot(em_df, aes(x=CNTR_ID, y=explained_var, fill=scenario))+  #fill=model
#    geom_bar(stat="identity", position = "dodge2")+
#    facet_grid(scenario~.)
# ggplot(em_df, aes(x=NAME_ENGL, y=explained_var, fill=scenario))+  #fill=model
#    geom_bar(stat="identity", position = "dodge2")+
#    labs(title=target_poll)+
#    coord_flip()
# 
# # Netherlands
# ggplot(em_df %>% filter(NAME_ENGL=='Netherlands'), 
#        aes(x=scenario, y=explained_var))+  #fill=model
#    geom_bar(stat="identity", position = "dodge2")+
#    coord_flip()


# ggsave(paste0('results/figures/escape_r2_', target_poll,'.tiff'), 
#        width = 8, height = 6, units = 'in', dpi = 300)
# if(target_poll=='NO2'){
# 
#    ggplot(em_df %>% dplyr::select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')),
#           aes(x=scenario, y=values, fill=model))+  #fill=model
#       geom_bar(stat="identity", position = "dodge2")+
#       # facet_grid(EM~., scales ='free')+
#       labs(y='RMSE', title = target_poll)+
#       coord_flip()+
#       theme(axis.title = element_text(size = 13),
#             axis.text = element_text(size = 11),
#             legend.title = element_text(size = 13),
#             legend.text = element_text(size = 13),
#             strip.text.y = element_text(size = 12))+
#       geom_hline(aes(yintercept=elapseRMSE))+
#       lims(y=c(0,13))
# }else{
# 
#    ggplot(em_df %>% dplyr::select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')),
#           aes(x=scenario, y=values, fill=model))+  #fill=model
#       geom_bar(stat="identity", position = "dodge2")+
#       # facet_grid(EM~., scales ='free')+
#       labs(y='RMSE', title = target_poll)+
#       coord_flip()+
#       theme(axis.title = element_text(size = 13),
#             axis.text = element_text(size = 11),
#             legend.title = element_text(size = 13),
#             legend.text = element_text(size = 13),
#             strip.text.y = element_text(size = 12))+
#       geom_hline(aes(yintercept=elapseRMSE))
# }
# ggsave(paste0('results/figures/escape_RMSE_', target_poll,'.tiff'), 
#        width = 8, height = 6, units = 'in', dpi = 300)
# 
# ### Give NO2 performance matrix for stations with PM25 as well
# 
# no2 <- read.csv('data/processed/prediction_escape_fromR_NO2', header=T)
# pm25 <- read.csv('data/processed/prediction_escape_fromR_PM2.5', header=T)
# no2sub <- no2[no2$allid%in%pm25$allid, ]
# exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo')
# no2sub <- no2sub[,!(names(no2sub)%in%exc_names)]
# no2_clean <- no2sub[,grepl('slr|rf|gwr', names(no2sub))]
# no2_clean <- no2_clean[, !grepl('no2lurfull', names(no2_clean))]
# names(no2_clean)
# model_years <- lapply(strsplit(names(no2_clean), "_"), `[[`, 3) %>% unlist
# # Select predictions for the year 2010
# no2_clean <- no2_clean[, model_years=='2010']
# models_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 1) %>% unlist
# # substr(names(no2_clean), nchar(names(no2_clean))-9, nchar(names(no2_clean))-5) 
# 
# no2_clean <- no2_clean[, order(models_name)]
# scenarios_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 2) %>% unlist
# 
# em_df <- lapply(names(no2_clean), function(pred_name){
#    scenario_name <- substr(pred_name, 1, nchar(pred_name)-5)
#    model_name <- unlist(strsplit(pred_name, '_'))[1]
#    error_matrix(no2sub[, 'no2'], no2sub[, pred_name]) %>% 
#       t() %>% 
#       as.data.frame() %>% 
#       mutate(model=model_name, scenario=scenario_name)
# }) %>% do.call(rbind, . )
# 
# ggplot(em_df %>% dplyr::select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
#        aes(x=scenario, y=values, fill=model))+  #fill=model
#    geom_bar(stat="identity", position = "dodge2")+
#    # facet_grid(EM~., scales ='free')+
#    labs(y='R2')+
#    coord_flip()+
#    theme(axis.title = element_text(size = 13),
#          axis.text = element_text(size = 11),
#          legend.title = element_text(size = 13),
#          legend.text = element_text(size = 13),
#          strip.text.y = element_text(size = 12))
# ggsave(paste0('results/figures/escape_r2_no2Withpm25.tiff'), 
#        width = 7, height = 7, units = 'in', dpi = 600)
# 
# 
# ggplot(em_df %>% dplyr::select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
#        aes(x=scenario, y=values, fill=model))+  #fill=model
#    geom_bar(stat="identity", position = "dodge2")+
#    # facet_grid(EM~., scales ='free')+
#    labs(y='RMSE')+
#    coord_flip()+
#    theme(axis.title = element_text(size = 13),
#          axis.text = element_text(size = 11),
#          legend.title = element_text(size = 13),
#          legend.text = element_text(size = 13),
#          strip.text.y = element_text(size = 12))
# ggsave(paste0('results/figures/escape_RMSE_no2Withpm25.tiff'), 
#        width = 7, height = 7, units = 'in', dpi = 600)

