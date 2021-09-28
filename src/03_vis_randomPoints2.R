library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
library(sf)
library(data.table)
target_polls <- c('NO2', 'PM25', 'PM10')
target_polls2 <- c('NO2', 'PM2.5', 'PM10')
escape_obss <- c('no2', 'pm25', 'pm10')


poll_i <- 1
target_poll <- target_polls[poll_i]      #'NO2'    # PM25
target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
escape_obs <- escape_obss[poll_i]  #'no2'


# SLR, GWR
files <- list.files('data/processed/gee/', 'predictionsAll_random')
files <- files[grepl(target_poll, files)]
# Multiple-year RF
files2 <- list.files('data/workingData/', 'RF_result_validation_random_')
files2 <- files2[grepl(target_poll2, files2)]
# Single-year RF
files3 <- list.files('../EXPANSE_algorithm/data/workingData/', 'RF_result_validation_random_')
files3 <- files3[grepl(target_poll2, files3)]

no2_l <- lapply(paste0('data/processed/gee/', files[!grepl('RF', files)]), 
                read.csv)  #
no2_linear <- do.call(rbind, no2_l)
no2_linear$system.index <- unlist(lapply(strsplit(no2_linear$system.index, '_'), `[[`, 1))
# Multiple-year modelling

tests_mrf <- fread(paste0('data/workingData/', files2[1]))
names(tests_mrf)
# Exclude those outside the year period
files2
no2_rf_pure_l <- lapply(paste0('data/workingData/', files2), 
                        function(mrf_file){
                           mrf <- fread(mrf_file)
                           period <- gsub('.csv', '', unlist(strsplit(mrf_file, '_'))[6])
                           scenario_name <- paste0('rf_', gsub('-', '.', period))   # eg: rf_00.19
                           
                           startYr <- as.numeric(paste0('20', strsplit(period, '-')[[1]][1]))
                           endYr <- as.numeric(paste0('20', strsplit(period, '-')[[1]][2]))
                           mrf <- mrf %>% filter(year%in%(startYr:endYr)) %>% rename(setNames('rf', scenario_name))
                           mrf$system.index <- unlist(lapply(strsplit(mrf$system.index, '_'), `[[`, 2))
                           mrf_wide <- mrf  %>%  pivot_wider(., id_cols = system.index, 
                                                             names_from = year, 
                                                             values_from = scenario_name)
                           names(mrf_wide)[2:ncol(mrf_wide)] <- paste0(scenario_name, '_', names(mrf_wide)[2:ncol(mrf_wide)])
                           mrf_wide
                        })  #add a index

# Single-year modelling
# Select for that specific year
no2_rf_pure_l3 <- lapply(paste0('../EXPANSE_algorithm/data/workingData/', files3), 
                         function(srf_file){
                            srf <- fread(srf_file)
                            yr <- gsub('.csv', '', unlist(strsplit(srf_file, '_'))[7]) %>% as.numeric()
                            scenario_name <- paste0('rf_', gsub('-', '.', yr), '_', yr)   # eg: rf_2000
                            srf <- srf %>% filter(year==yr) %>% rename(setNames('rf', scenario_name))
                            srf$system.index <- unlist(lapply(strsplit(srf$system.index, '_'), `[[`, 2))
                            srf
                         })  #add a index

# Combine all dataframe in the list
no2_rf_pure <- Reduce(
   function(x, y, ...) inner_join(x, y,  by=c('system.index'), ...), 
   no2_rf_pure_l%>% 
      lapply(., function(df_all) df_all %>% dplyr::select(system.index, matches('rf_')))
)

no2_rf_pure3 <- Reduce(
   function(x, y, ...) inner_join(x, y,  by=c('system.index'), ...), 
   no2_rf_pure_l3%>% 
      lapply(., function(df_all) df_all %>% dplyr::select(system.index, matches('rf_')))
)


no2 <- inner_join(no2_linear, no2_rf_pure, by=names(no2_linear)[names(no2_linear)%in%names(no2_rf_pure)])
no2 <- inner_join(no2, no2_rf_pure3, by=names(no2)[names(no2)%in%names(no2_rf_pure3)])

fwrite(no2, paste0('data/processed/prediction_random_fromR_', target_poll2), row.names=F)

names(no2)
exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo',
               'b1')

no2_clean <- no2[,!(names(no2)%in%exc_names)]
no2_clean <- no2_clean[,!grepl('gtwr', names(no2_clean))]
names(no2_clean)
no2_clean_pure <- no2_clean %>% dplyr::select(-'cntr_id', -'cntr_name')
no2_clean_name <- no2_clean %>% dplyr::select('cntr_id', 'cntr_name')
yrs_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-3, nchar(names(no2_clean_pure))) %>% as.numeric()
no2_clean_pure <- no2_clean_pure[, order(yrs_name)]
scenarios_name <- paste0(unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 1)), '_', unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 2)))
no2_clean_pure <- no2_clean_pure[, order(scenarios_name)]
# no2_clean <- no2_clean[, mixedsort(names(no2_clean))]

names(no2_clean_pure)
yr=2018
no2_clean_p <- no2_clean[, grepl(yr, names(no2_clean))]
# gather(no2_clean_p, 'model', 'values', )

# data_df <- no2_clean[, grepl('2018', names(no2_clean))]

plotM <- function(data_df, colorZone=F, lim_range, gtitle){
   names(data_df) <- substr(names(data_df), 1, nchar(names(data_df))-4)
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
              upper = list(continuous = wrap("cor", size=8)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)),
              mapping=ggplot2::aes(colour=zone.name), title=gtitle
      )
   }else{
      ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
              upper = list(continuous = wrap("cor", size=8)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)), 
              title=gtitle
      )
   }
   
}
# png(paste0("graph/randomPoints", target_poll, '.png'),
#      height=12, width=15, units='in', res=300)
# plotM(no2_clean, F, range_limit, '2008')
# dev.off()
if(target_poll=='NO2'){
   range_limit <- c(0, 125)
}else if(target_poll=='O3'){
   range_limit <- c(0, 120)
}else{
   range_limit <- c(0, 35)
}

plotM(no2_clean[, grepl('2018', names(no2_clean))], F, range_limit, '2018')+
   theme(strip.placement = "outside", text = element_text(size = 13))

plotM(no2_clean[, grepl('rf', names(no2_clean))], F, range_limit, 'rf')

plotM(no2_clean[, grepl('2012', names(no2_clean))], F, range_limit, '2012')
plotM(no2_clean[, grepl('2009', names(no2_clean))], F, range_limit, '2009')
plotM(no2_clean[, grepl('2000', names(no2_clean))], F, range_limit, '2000')

# png(paste0("graph/randomPoints", target_poll, '_slr.png'),
#     height=12, width=15, units='in', res=300)
# plotM(no2_clean[, grepl('slr', names(no2_clean))], F, range_limit)
# dev.off()

png(paste0("graph/randomPoints", target_poll, '_gwr.png'),
    height=12, width=15, units='in', res=100)
plotM(no2_clean[, grepl('gwr', names(no2_clean))], F, range_limit, 'GWR')+
   theme(strip.placement = "outside", text = element_text(size = 13))
dev.off()

png(paste0("graph/randomPoints", target_poll, '_rf.png'),
    height=12, width=15, units='in', res=100)
plotM(no2_clean[, grepl('rf', names(no2_clean))], F, range_limit, 'RF')+
   theme(strip.placement = "outside", text = element_text(size = 13))
dev.off()


### Plot the scatterplots of predictions at random points
## from different algorithms.
files <- list.files('data/processed/gee/', 'predictionsAll_random')
files <- files[grepl(target_poll, files)]
no2_l <- lapply(paste0('data/processed/gee/', files[!grepl('RF', files)]), 
                read.csv)  #
no2_linear <- do.call(rbind, no2_l)
no2_rf_pure_l <- lapply(paste0('data/processed/gee/', files[grepl('RF', files)]), 
                        read.csv)  #add a index


# There is one missing value in RF_2019...
if(any(unlist(lapply(no2_rf_pure_l, nrow))!=50887)){
   no2_rf_pure <- Reduce(
      function(x, y, ...) inner_join(x, y,  by=c('system.index', 'cntr_name', 'cntr_id'), ...), 
      no2_rf_pure_l[unlist(lapply(no2_rf_pure_l, nrow))==50887] %>% 
         lapply(., function(df_all) df_all %>% select(system.index, cntr_name, 
                                                      cntr_id, matches('rf_')))
   )
   
   no2 <- inner_join(no2_rf_pure, no2_rf_pure_l[unlist(lapply(no2_rf_pure_l, nrow))!=50887] %>%
                        lapply(., function(df_all) df_all %>% select(system.index, matches('rf_'))) %>% 
                        Reduce(
                           function(x, y, ...) inner_join(x, y,  by='system.index', ...), 
                           .
                        )
                     ,
                     by='system.index')
   no2_clean <- inner_join(no2_linear, no2, by=names(no2_linear)[names(no2_linear)%in%names(no2)])
}else{
   no2 <- Reduce(
      function(x, y, ...) inner_join(x, y,  by=c('system.index', 'cntr_name', 'cntr_id'), ...), 
      no2_rf_pure_l[unlist(lapply(no2_rf_pure_l, nrow))==50887] %>% 
         lapply(., function(df_all) df_all %>% select(system.index, cntr_name, 
                                                      cntr_id, matches('rf_')))
   )
   no2_clean <- inner_join(no2_linear, no2, by=names(no2_linear)[names(no2_linear)%in%names(no2)])
}
exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo',
               'b1')
no2_clean <- no2_clean[,!(names(no2_clean)%in%exc_names)]
no2_clean <- no2_clean[,!grepl('gtwr', names(no2_clean))]
names(no2_clean)
no2_clean_pure <- no2_clean %>% select(-'cntr_id', -'cntr_name')
no2_clean_name <- no2_clean %>% select('cntr_id', 'cntr_name')
yrs_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-3, nchar(names(no2_clean_pure))) %>% as.numeric()
no2_clean_pure <- no2_clean_pure[, order(yrs_name)]
scenarios_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-9, nchar(names(no2_clean_pure))-5) 
no2_clean_all <- no2_clean_pure[, order(scenarios_name)]

# GWR vs RF (2012)
yrs_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-3, nchar(names(no2_clean_pure))) %>% as.numeric()
no2_2010 <- cbind(no2_clean_name, no2_clean_pure[, yrs_name==2012])
corr <- lapply(unique(no2_2010$cntr_id), function(cntr_id){
   cor((no2_2010 %>% filter(cntr_id==cntr_id))[, -c(1,2)])
})
# the correlation does not vary with country
lapply(corr, `[[`, 2) %>% unlist
#
plotM(no2_clean_all[, grepl(2018, names(no2_clean_all))], F, range_limit, 2018)+
   theme(strip.placement = "outside", text = element_text(size = 13))
plotM(no2_clean_all[, grepl(2012, names(no2_clean_all))], F, range_limit, 2012)+
   theme(strip.placement = "outside", text = element_text(size = 13))
plotM(no2_clean_all[, grepl(2009, names(no2_clean_all))], F, range_limit, 2009)+
   theme(strip.placement = "outside", text = element_text(size = 13))
plotM(no2_clean_all[, grepl('2000', names(no2_clean_all))], F, range_limit)
for(yr in 2000:2018){
   print(yr)
   png(paste0("results/figures/randomPoints", target_poll, '_', yr,'.png'),
       height=12, width=15, units='in', res=300)
   print(plotM(no2_clean_all[, grepl(yr, names(no2_clean_all))], F, range_limit, yr)+
            theme(strip.placement = "outside", text = element_text(size = 17)))
   dev.off()
}
# Divide by country
unique(no2_clean$cntr_name)
unique(no2_clean$cntr_id)
no2_clean %>% names()
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
