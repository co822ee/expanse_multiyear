library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
library(sf)
target_poll <- 'PM25'
escape_obs <- 'pm25'
elapseR2 <- 0.494          ## 0.494 for NO2 and 0.648 for pm25
elapseRMSE <- 11.47          ### 11.47 for no2 3.41 for pm25
# files <- paste0('data/processed/gee/predictionsAll_escape', target_poll,'.csv')
files <- list.files('data/processed/gee/', 'predictionsAll_escape')
files <- files[grepl(target_poll, files)]
no2_l <- lapply(paste0('data/processed/gee/', files[!grepl('RF', files)]), 
              read.csv)  #
no2_linear <- no2_l[[1]]
no2_rf_pure_l <- lapply(paste0('data/processed/gee/', files[grepl('RF', files)]), 
                 read.csv)  #add a index

# There is one missing value in RF_2019...

if(any(unlist(lapply(no2_rf_pure_l, nrow))!=max(unlist(lapply(no2_rf_pure_l, nrow))))){
   no2_rf_pure <- Reduce(
      function(x, y, ...) inner_join(x, y,  by=c('system.index'), ...), 
      no2_rf_pure_l[unlist(lapply(no2_rf_pure_l, nrow))==max(unlist(lapply(no2_rf_pure_l, nrow)))] %>% 
         lapply(., function(df_all) df_all %>% select(system.index, matches('rf_')))
   )
   
   no2 <- inner_join(no2_rf_pure, no2_rf_pure_l[unlist(lapply(no2_rf_pure_l, nrow))!=max(unlist(lapply(no2_rf_pure_l, nrow)))] %>%
                        lapply(., function(df_all) df_all %>% select(system.index, matches('rf_'))) %>% 
                        Reduce(
                           function(x, y, ...) inner_join(x, y,  by='system.index', ...), 
                           .
                        )
                     ,
                     by='system.index')
   no2 <- inner_join(no2_linear, no2, by=names(no2_linear)[names(no2_linear)%in%names(no2)])
}else{
   no2 <- Reduce(
      function(x, y, ...) inner_join(x, y,  by=c('system.index'), ...), 
      no2_rf_pure_l[unlist(lapply(no2_rf_pure_l, nrow))==max(unlist(lapply(no2_rf_pure_l, nrow)))] %>% 
         lapply(., function(df_all) df_all %>% select(system.index,  matches('rf_')))
   )
   no2 <- inner_join(no2_linear, no2, by=names(no2_linear)[names(no2_linear)%in%names(no2)])
}

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

names(no2_clean)


if(target_poll=='NO2'){
   range_limit <- c(0, 140)
}else{
   range_limit <- c(0, 45)
}
## Divide by country
if(target_poll=='NO2'){
   raw_df <- read.csv('data/raw/NO2_ESCAPE_2010_Feb2018_updated_for_SGR_without_SPB.csv')
}else{
   raw_df <- read.csv('data/raw/PM25_ESCAPE_2010_Feb2018_excluding_SGR.csv')
}
no2 <- inner_join(no2, raw_df %>% select(allid, x_etrs, y_etrs), by='allid')
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
ggplot(em_df, aes(x=CNTR_ID, y=explained_var, fill=scenario))+  #fill=model
   geom_bar(stat="identity", position = "dodge2")+
   facet_grid(scenario~.)
ggplot(em_df, aes(x=NAME_ENGL, y=explained_var, fill=scenario))+  #fill=model
   geom_bar(stat="identity", position = "dodge2")+
   labs(title=target_poll)+
   coord_flip()
   

lapply(unique(em_df$scenario), function(model_a){
   em_df %>% filter(scenario==model_a) %>% select(scenario, explained_var, NAME_ENGL)
})

table(no2$X.code)
### 
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
   ggplot(em_df %>% select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
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
}else{
   ggplot(em_df %>% select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
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
}
ggsave(paste0('results/figures/escape_r2_', target_poll,'.tiff'), 
       width = 8, height = 6, units = 'in', dpi = 300)
if(target_poll=='NO2'){
   
   ggplot(em_df %>% select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
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
}else{
   
   ggplot(em_df %>% select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
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
}
ggsave(paste0('results/figures/escape_RMSE_', target_poll,'.tiff'), 
       width = 8, height = 6, units = 'in', dpi = 300)

### Give NO2 performance matrix for stations with PM25 as well

no2 <- read.csv('data/processed/gee/predictionsAll_escape_NO2.csv', header=T)
pm25 <- read.csv('data/processed/gee/predictionsAll_escape_PM25.csv', header=T)
no2sub <- no2[no2$allid%in%pm25$allid, ]
exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo')
no2sub <- no2sub[,!(names(no2sub)%in%exc_names)]
no2_clean <- no2sub[,grepl('slr|rf|gwr', names(no2sub))]
no2_clean <- no2_clean[, !grepl('no2lurfull', names(no2_clean))]
names(no2_clean)
# yrs_name <- substr(names(no2_clean), nchar(names(no2_clean))-3, nchar(names(no2_clean))) %>% as.numeric()
# no2_clean <- no2_clean[, order(yrs_name)]
models_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 1) %>% unlist
# substr(names(no2_clean), nchar(names(no2_clean))-9, nchar(names(no2_clean))-5) 

no2_clean <- no2_clean[, order(models_name)]
scenarios_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 2) %>% unlist

em_df <- lapply(names(no2_clean), function(pred_name){
   scenario_name <- substr(pred_name, 1, nchar(pred_name)-5)
   model_name <- unlist(strsplit(pred_name, '_'))[1]
   error_matrix(no2sub[, 'no2'], no2sub[, pred_name]) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(model=model_name, scenario=scenario_name)
}) %>% do.call(rbind, . )
ggplot(em_df %>% select(rsq, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
       aes(x=scenario, y=values, fill=model))+  #fill=model
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   labs(y='R2')+
   coord_flip()+
   theme(axis.title = element_text(size = 13),
         axis.text = element_text(size = 11),
         legend.title = element_text(size = 13),
         legend.text = element_text(size = 13),
         strip.text.y = element_text(size = 12))+
   lims(y=c(0,0.6))
ggsave(paste0('results/figures/escape_r2_no2Withpm25.tiff'), 
       width = 7, height = 7, units = 'in', dpi = 600)


ggplot(em_df %>% select(RMSE, model, scenario) %>%  gather("perfm", "values", -c("model", 'scenario')), 
       aes(x=scenario, y=values, fill=model))+  #fill=model
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   labs(y='RMSE')+
   coord_flip()+
   theme(axis.title = element_text(size = 13),
         axis.text = element_text(size = 11),
         legend.title = element_text(size = 13),
         legend.text = element_text(size = 13),
         strip.text.y = element_text(size = 12))+
   lims(y=c(0,13))
ggsave(paste0('results/figures/escape_RMSE_no2Withpm25.tiff'), 
       width = 7, height = 7, units = 'in', dpi = 600)

