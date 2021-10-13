library(dplyr)
library(tidyr)
library(gtools)
library(reshape2)
library(ggplot2)
library(GGally)
library(sf)
library(data.table)
library(tmap)
target_polls <- c('NO2', 'PM25', 'PM10', 'O3')
target_polls2 <- c('NO2', 'PM2.5', 'PM10', 'O3')




for(poll_i in 1:4){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   
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
   rm(list=c('no2_rf_pure_l', 'no2_rf_pure_l3', 'no2_l'))
}
#---------- AR plots --------
calc_acf <- function(dat, varname, time_step){
   out_dat <- acf(dat[, varname], plot=F, lag.max=(time_step-1)*12)$acf[-1]
   out_dat <- as.data.frame(t(out_dat))
   names(out_dat) <- paste0('ar_', 1:length(out_dat))
   out_dat
}
acf_timeseries <- function(dat, target_str, target_poll){
   
   # From wide to long data table
   no2_wide <- cbind(dat[, c('system.index', 'cntr_id', 'cntr_name')], 
                     dat[, grepl(target_str, names(dat))])
   no2_long <- no2_wide %>% pivot_longer(., cols = -c('system.index', 'cntr_id', 'cntr_name'),
                                         names_to = 'models',
                                         values_to = 'predictions')
   no2_long$year <- as.numeric(unlist(lapply(strsplit(no2_long$models, '_'), `[[`, 3)))
   
   no2_long <- no2_long %>% 
      arrange(system.index, year)  # The data needs to be arranged from early times to later time
   tperiod <- sort(unique(no2_long$year))
   # dat_tbl <- table(dat$system.index)
   acf_dat <- no2_long%>%
      # filter(system.index%in%names(dat_tbl[dat_tbl==max(dat_tbl)])) %>% 
      group_by(system.index) %>%
      do(calc_acf(., 'predictions', length(tperiod)))
   
   acf_avg <- acf_dat[-1] %>% apply(., 2, mean)
   # summarise(
   #    acf1=acf(obs, plot = F)$acf[-1][1],
   #    acf12=acf(obs, plot = F)$acf[-1][12],
   #    acf24=acf(obs, plot = F)$acf[-1][24]
   # )
   ggplot(acf_dat %>% gather('ar', 'values', -'system.index') %>% mutate(lag=as.numeric(gsub('ar_', '', ar))))+
      geom_line(aes(x=reorder(lag, lag), y=values, group=system.index), alpha=0.25)+
      geom_line(data=acf_avg %>% t()%>% as.data.frame()%>% gather('ar', 'values') %>% mutate(lag=as.numeric(gsub('ar_', '', ar))), 
                aes(x=reorder(lag, lag), y=values, group=1), col='red', lwd=1)+
      geom_text(data=acf_avg %>% 
                   t()%>% as.data.frame()%>% 
                   gather('ar', 'values') %>% mutate(lag=as.numeric(gsub('ar_', '', ar))) %>% 
                   filter(lag%in% c(1, seq(5, (5*length(tperiod)-1), 5))),
                aes(label=round(values, 1), x=lag+0.2, y=0.9), col='red')+
      geom_vline(xintercept = seq(5, (5*length(tperiod)-1), 5),
                 lty=2)+
      labs(title=paste0(target_str, ' (', target_poll, ') ', min(tperiod), '-', max(tperiod)),
           subtitle=paste0('no. of stations=', nrow(acf_dat)),
           x='lag (year)', y='autocorrelation')+
      scale_x_discrete(breaks = c(1, seq(5, (12*length(tperiod)-1), 5)))
   # geom_abline(intercept = 0, slope=1)
}
plot_ar <- function(poll_i){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   
   no2 <- fread(paste0('data/processed/prediction_random_fromR_', target_poll2)) %>% as.data.frame()
   exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo',
                  'b1')
   acf_timeseries(no2, 'slr_20', target_poll)
   acf_timeseries(no2, 'gwr_20', target_poll)
   acf_timeseries(no2, 'rf_20', target_poll)
   
   acf_timeseries(no2, 'slr_00.19', target_poll)
   acf_timeseries(no2, 'rf_00.19', target_poll)
   
   
   # no2_clean_pure <- no2_clean %>% dplyr::select(-'cntr_id', -'cntr_name')
   # no2_clean_name <- no2_clean %>% dplyr::select('cntr_id', 'cntr_name')
   # yrs_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-3, nchar(names(no2_clean_pure))) %>% as.numeric()
   # no2_clean_pure <- no2_clean_pure[, order(yrs_name)]
   # scenarios_name <- paste0(unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 1)), '_', unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 2)))
   # no2_clean_pure <- no2_clean_pure[, order(scenarios_name)]
   
   
}
#------------ Correlation plots ------------
for(poll_i in 1:4){
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   
   
   no2 <- fread(paste0('data/processed/prediction_random_fromR_', target_poll2)) %>% as.data.frame()
   exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo',
                  'b1')
   
   no2_clean <- no2[, which(!(names(no2)%in%exc_names))]
   names(no2_clean)
   no2_clean_pure <- no2_clean %>% dplyr::select(-'cntr_id', -'cntr_name')
   no2_clean_name <- no2_clean %>% dplyr::select('cntr_id', 'cntr_name')
   yrs_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-3, nchar(names(no2_clean_pure))) %>% as.numeric()
   no2_clean_pure <- no2_clean_pure[, order(yrs_name)]
   scenarios_name <- paste0(unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 1)), '_', unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 2)))
   no2_clean_pure <- no2_clean_pure[, order(scenarios_name)]
   
   
   plotM <- function(data_df, colorZone=F, gtitle, sameYr=T){
      if(sameYr){
         names(data_df) <- paste0(unlist(lapply(strsplit(names(data_df), '_'), `[[`, 1)),
                                  unlist(lapply(strsplit(names(data_df), '_'), `[[`, 2)))
         # substr(names(data_df), 1, nchar(names(data_df))-4)
      }else{
         names(data_df) <- unlist(lapply(strsplit(names(data_df), '_'), `[[`, 3))
      }
      data_df <- data_df[, mixedorder(names(data_df))]
      lim_range <- c(min(data_df), max(data_df))
      
      lowerFn <- function(data, mapping, ...) {
         p <- ggplot(data = data, mapping = mapping) +
            geom_point(colour = "black") +
            geom_abline(intercept=0, slope=1, col='red')+
            lims(x=lim_range,y=lim_range)
         p
      }
      if(colorZone){
         ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
                 upper = list(continuous = wrap("cor", size=5)),
                 diag=list(discrete="barDiag", 
                           continuous = wrap("densityDiag", alpha=0.5)),
                 mapping=ggplot2::aes(colour=zone.name), title=gtitle
         )
      }else{
         ggpairs(data=data_df, lower = list(continuous = wrap(lowerFn)),
                 upper = list(continuous = wrap("cor", size=5)),
                 diag=list(discrete="barDiag", 
                           continuous = wrap("densityDiag", alpha=0.5)), 
                 title=gtitle
         )
      }
   }
  
   ## Plot predictions for different years using the same algorithm (year-wise)
   # # 1. single-year SLR
   # png(paste0("graph/randomPoints", target_poll, '_slr.png'),
   #     height=12, width=16.5, units='in', res=300)
   # plotM(no2_clean[, grepl('slr_20', names(no2_clean))], F, 'Single-year SLR', F)+
   #    theme(strip.placement = "outside", text = element_text(size = 13))
   # dev.off()
   # 
   # # 2. single-year GWR
   # png(paste0("graph/randomPoints", target_poll, '_gwr.png'),
   #     height=12, width=16.5, units='in', res=100)
   # plotM(no2_clean[, grepl('gwr_20', names(no2_clean))], F, 'Single-year GWR', F)+
   #    theme(strip.placement = "outside", text = element_text(size = 13))
   # dev.off()
   # 
   # # 3. single-year random forests
   # png(paste0("graph/randomPoints", target_poll, '_rf.png'),
   #     height=12, width=16.5, units='in', res=100)
   # plotM(no2_clean[, grepl('rf_20', names(no2_clean))], F, 'Single-year RF', F)+
   #    theme(strip.placement = "outside", text = element_text(size = 13))
   # dev.off()
   # 
   # # 4. multiple-year SLR
   # png(paste0("graph/randomPoints", target_poll, '_mslr.png'),
   #     height=12, width=16.5, units='in', res=100)
   # plotM(no2_clean[, grepl('slr_00.19', names(no2_clean))], F, 'Multiple-year SLR', F)+
   #    theme(strip.placement = "outside", text = element_text(size = 13))
   # dev.off()
   # 
   # # 5. multiple-year random forests
   # png(paste0("graph/randomPoints", target_poll, '_mrf.png'),
   #     height=12, width=16.5, units='in', res=100)
   # plotM(no2_clean[, grepl('rf_00.19', names(no2_clean))], F, 'Multiple-year RF', F)+
   #    theme(strip.placement = "outside", text = element_text(size = 13))
   # dev.off()
   # 
   # # 6. multiple-year GTWR
   # png(paste0("graph/randomPoints", target_poll, '_gtwr.png'),
   #     height=12, width=16.5, units='in', res=100)
   # plotM(no2_clean[, grepl('gtwr_00.19', names(no2_clean))], F, 'multiple-year GTWR', F)+
   #    theme(strip.placement = "outside", text = element_text(size = 13))
   # dev.off()
   # 
   
   lapply(c(2000, 2005, 2010, 2015, 2019),
          function(yr){
             no2_clean_p <- no2_clean[, grepl(yr, names(no2_clean))]
             png(paste0("graph/randomPoints", target_poll, '_', yr, '_gtwr.png'),
                 height=8, width=7.8, units='in', res=100)
             plotM(no2_clean_p, F, yr, T)+
                theme(strip.placement = "outside", text = element_text(size = 13))
             dev.off()
          })
   
   ### Plot the scatterplots of predictions at random points
   ## from different algorithms.
   # mrf_wide <- lapply(3:ncol(no2_clean), function(col_index){
   #    target_df <- no2_clean[, c(1,2, col_index)] 
   #    target_df$year <- as.numeric(strsplit(names(target_df)[3], '_')[[1]][3])
   #    names(target_df)[3] <- paste0(strsplit(names(target_df)[3], '_')[[1]][1],
   #                                  '_',
   #                                  strsplit(names(target_df)[3], '_')[[1]][2])
   #    
   #    target_df%>% pivot_longer(., cols =  names(target_df)[3], names_to = 'model', 
   #                              values_to = 'prediction')
   # })
   # all_algorithms <- do.call(rbind, mrf_wide)
   # rm(mrf_wide)
   # names(all_algorithms)
   # all_algorithms$model %>% unique
   
   strs_v <- data.frame(str1='slr_2010_2010',
                        str2=c('slr_2019_2019', 
                               'slr_2015_2015',
                               'slr_2005_2005',
                               'slr_2000_2000'))
   strs_v <- data.frame(str1='rf_2010_2010',
                        str2=c('rf_2019_2019', 
                               'rf_2015_2015',
                               'rf_2005_2005',
                               'rf_2000_2000'))
   strs_v <- data.frame(str1='gwr_2010_2010',
                        str2=c('gwr_2019_2019', 
                               'gwr_2015_2015',
                               'gwr_2005_2005',
                               'gwr_2000_2000'))
   
   lapply(1:nrow(strs_v), function(i){
      str1 <- strs_v[i, 1]
      str2 <- strs_v[i, 2]
      cor_v <- lapply(unique(no2_clean$cntr_id), function(target_cntr){
         target_df <- no2_clean[no2_clean$cntr_id==target_cntr, ]
         data.frame(cor=cor(target_df[, -c(1,2)])[str1, str2],
                    cntr_id=target_df$cntr_id[1],
                    cntr_name=target_df$cntr_name[1]) %>% 
            mutate(r2=cor*cor) %>% 
            rename(setNames('cor', paste0('cor (', str1, ' vs ', str2, ')')),
                   setNames('r2', paste0('r2 (', str1, ' vs ', str2, ')')))
      }) %>% do.call(rbind, .)
      eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
      unique(eu_bnd$CNTR_ID) %>% sort()
      names(cor_v) <- toupper(names(cor_v))
      cor_v <- cor_v %>% select(-CNTR_NAME)
      cor_v_sp <- left_join(eu_bnd, cor_v, by=c('CNTR_ID'))
      # plot(cor_v_sp[6], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
      # plot(cor_v_sp[7], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
      
      # tm_shape(cor_v_sp)+
      #    tm_polygons(col=names(cor_v_sp)[6],
      #                style = "fixed",
      #                breaks = seq(0.2, 1, 0.2))+
      #    tm_layout(legend.outside = TRUE) 
      r2tmap <- tm_shape(cor_v_sp)+
         tm_polygons(col=names(cor_v_sp)[7],
                     style = "fixed",
                     breaks = seq(0.2, 1, 0.2),
                     palette = c("#FF0000", "#FFA500", '#FFFF00', "#8FBC8F", "#4682B4"))+
         tm_layout(legend.outside = TRUE) 
      tmap_save(r2tmap, paste0('graph/rp_cntr_', target_polls2, '_', str1, str2, '.tiff'),
                dpi=150, height=4, width=6, units='in')
   })
   
}

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
       height=12, width=16.5, units='in', res=300)
   print(plotM(no2_clean_all[, grepl(yr, names(no2_clean_all))], F, range_limit, yr)+
            theme(strip.placement = "outside", text = element_text(size = 17)))
   dev.off()
}
# Divide by country
unique(no2_clean$cntr_name)
unique(no2_clean$cntr_id)
no2_clean %>% names()
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
