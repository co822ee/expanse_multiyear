library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
target_poll <- 'NO2'
escape_obs <- 'no2'
elapseR2 <- 0.494          ## 0.494 for NO2 and 0.648 for pm25
elapseRMSE <- 11.47          ### 11.47 for no2 3.41 for pm25
files <- paste0('data/processed/gee/predictionsAll_escape_', target_poll,'.csv')
no2 <- read.csv(files, header=T)
exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo')
no2 <- no2[,!(names(no2)%in%exc_names)]
no2_clean <- no2[,grepl('slr|rf|gwr', names(no2))]
no2_clean <- no2_clean[, !grepl('no2lurfull', names(no2_clean))]
names(no2_clean)
# yrs_name <- substr(names(no2_clean), nchar(names(no2_clean))-3, nchar(names(no2_clean))) %>% as.numeric()
# no2_clean <- no2_clean[, order(yrs_name)]
models_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 1) %>% unlist
   # substr(names(no2_clean), nchar(names(no2_clean))-9, nchar(names(no2_clean))-5) 

no2_clean <- no2_clean[, order(models_name)]
scenarios_name <- lapply(strsplit(names(no2_clean), "_"), `[[`, 2) %>% unlist
# no2_clean <- no2_clean[, order(scenarios_name)]
# no2_clean <- no2_clean[, mixedsort(names(no2_clean))]

names(no2_clean)

plotM <- function(data_df, colorZone=F, lim_range, gtitle){
   names(data_df) <- c(names(data_df)[1], substr(names(data_df)[-1], 1, nchar(names(data_df)[-1])-5))
   # data_df <- dcast(data_df,
   #                    # as.formula(paste0('station_european_code+zone.name~', time_var)), 
   #                    value.var = 'conc')
   
   upper.panel <- function(x, y){
      points(x, y,xlim=c(-2,220), ylim=c(-2,220))
      abline(0,1, col='red')
   }
   lowerFn <- function(data, mapping, ...) {
      p <- ggplot(data = data, mapping = mapping) +
         geom_point(colour = "black") +
         geom_abline(intercept=0, slope=1, col='red')+
         lims(x=lim_range,y=lim_range)
      p
   }
   if(colorZone){
      ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
              upper = list(continuous = wrap("cor", size=6)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)),
              mapping=ggplot2::aes(colour=zone.name), title=gtitle,
      )
   }else{
      ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
              upper = list(continuous = wrap("cor", size=6)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)), 
              title=gtitle,
      )
   }
   
}
# png(paste0("graph/randomPoints", target_poll, '.png'),
#      height=12, width=15, units='in', res=300)
# plotM(no2_clean, F, range_limit)
# dev.off()
if(target_poll=='NO2'){
   range_limit <- c(0, 140)
}else{
   range_limit <- c(0, 45)
}
### Scatterplots between the 
# plotM(no2[, c(escape_obs, names(no2_clean))], F, range_limit, '2010')+
#    theme(strip.placement = "outside", text = element_text(size = 13))

# png(paste0("results/figures/escapePoints", target_poll, '.png'),
#     height=12, width=15, units='in', res=300)
# plotM(no2[, c(escape_obs, names(no2_clean))], F, range_limit, '2010')+
#    theme(strip.placement = "outside", text = element_text(size = 13))
# dev.off()

### 
library(APMtools)
# Remove na in no2 (escape )
no2[is.na(no2$rf_00.19_2010), ] %>% View ## Dont know why there is one station with NA values for RF
no2 <- no2[!is.na(no2$rf_00.19_2010), ]

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
      labs(y='R2')+
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
      labs(y='R2')+
      coord_flip()+
      theme(axis.title = element_text(size = 13),
            axis.text = element_text(size = 11),
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 13),
            strip.text.y = element_text(size = 12))+
      geom_hline(aes(yintercept=elapseR2))
}
ggsave(paste0('results/figures/escape_r2_', target_poll,'.tiff'), 
       width = 7, height = 7, units = 'in', dpi = 600)
if(target_poll=='NO2'){
   
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
      geom_hline(aes(yintercept=elapseRMSE))+
      lims(y=c(0,13))
}else{
   
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
      geom_hline(aes(yintercept=elapseRMSE))
}
ggsave(paste0('results/figures/escape_RMSE_', target_poll,'.tiff'), 
       width = 7, height = 7, units = 'in', dpi = 600)

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

