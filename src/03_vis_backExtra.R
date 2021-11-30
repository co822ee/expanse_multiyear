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
library(doParallel)
library(foreach)
target_polls <- c('NO2', 'PM25', 'PM10', 'O3')
target_polls2 <- c('NO2', 'PM2.5', 'PM10', 'O3')

##########--------------Output data -------------- ##########
cluster_no <- 4
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(poll_i = seq_along(target_polls))  %dopar%  {
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
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   files <- list.files('data/processed/gee/', 'predictionsAll_random')
   files <- files[grepl(target_poll, files)]
   # Only model predictions
   files1 <- files[!grepl('backExtra', files)]
   files2 <- files[grepl('backExtra', files)]
   
   no2_lurL <- lapply(paste0('data/processed/gee/', files1), 
                      read.csv)  #
   no2_lur <- lapply(no2_lurL, function(dat){
      dat %>% dplyr::select(-x, -.geo)
   }) %>% do.call(rbind, .)
   no2_lur$system.index <- unlist(lapply(strsplit(no2_lur$system.index, '_'), `[[`, 1))
   
   
   no2_backL <- lapply(paste0('data/processed/gee/', files2), 
                       read.csv)  #
   no2_back <- lapply(no2_backL, function(dat){
      dat %>% dplyr::select(-x, -.geo)
   }) %>% do.call(rbind, .)
   no2_back$system.index <- unlist(lapply(strsplit(no2_back$system.index, '_'), `[[`, 1))
   # Reason of missing data in the back-extrapolated values:
   ## DEHM data is not available in Iceland (1263 points).
   ## DEHM data is not available at one point in the north tip of Norway (1 point).
   # no2_lur %>% 
   #    filter(system.index%in%(no2_lur$system.index[(!no2_lur$system.index%in%no2_back$system.index)])) %>% 
   #    select(nuts_id, cntr_id) %>% 
   #    unique()
   # no2_lur %>% 
   #    filter(system.index%in%(no2_lur$system.index[(!no2_lur$system.index%in%no2_back$system.index)])) %>% 
   #    filter(nuts_id=='NO0') %>% 
   #    nrow()
   
   # Remove the points where backextrapolated values are unavailable
   no2_lurNew <- no2_lur[which(no2_lur$system.index%in%no2_back$system.index), ]
   no2_all <- inner_join(no2_lurNew, no2_back, by=names(no2_lurNew)[names(no2_lurNew)%in%names(no2_back)])
   fwrite(no2_all, paste0('data/processed/prediction_random_extrapoAll_', target_poll2), row.names=F)
   
}
stopCluster(cl)
gc()
##########--------------Boxplot -------------- ##########
cluster_no <- 4
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(poll_i = seq_along(target_polls))  %dopar%  {
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
   
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   no2 <- fread(paste0('data/processed/prediction_random_extrapoAll_', target_poll2)) %>% 
      as.data.frame()
   
   exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo',
                  'b1')
   
   no2_clean <- no2[, which(!(names(no2)%in%exc_names))]
   no2_clean <- no2_clean[, !grepl('08.10|09.11|10.12|08.12|06.12|12.19', names(no2_clean))]
   names(no2_clean)
   no2_clean_pure <- no2_clean %>% dplyr::select(-'cntr_id', -'nuts_id')
   no2_clean_name <- no2_clean %>% dplyr::select('cntr_id', 'nuts_id')
   yrs_name <- substr(names(no2_clean_pure), nchar(names(no2_clean_pure))-3, nchar(names(no2_clean_pure))) %>% as.numeric()
   no2_clean_pure <- no2_clean_pure[, order(yrs_name)]
   scenarios_name <- paste0(unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 1)), '_', unlist(lapply(strsplit(names(no2_clean_pure), '_'), `[[`, 2)))
   no2_clean_pure <- no2_clean_pure[, order(scenarios_name)]
   
   yrs <- c(2000, 2005, 2010, 2015, 2019)
   no2_yr <- no2_clean[, grepl(paste(yrs, collapse = '|'), names(no2_clean))]
   
   data_df <- no2_yr
   
   # names(data_df) <- paste0(unlist(lapply(strsplit(names(data_df), '_'), `[[`, 1)),
   #                          '_',
   #                          unlist(lapply(strsplit(names(data_df), '_'), `[[`, 2)))
   data_df <- data_df[, mixedorder(names(data_df))]
   lim_range <- c(min(data_df), max(data_df))
   dat_long <- data_df %>% pivot_longer(., cols = names(data_df), 
                                        names_to = 'method', values_to = 'values')
   dat_long$scenario <- lapply(strsplit(dat_long$method, '_'), `[[`, 2) %>% unlist
   dat_long$model <- (lapply(strsplit(dat_long$method, '_'), `[[`, 1) %>% unlist)
   dat_long$year <- as.numeric(lapply(strsplit(dat_long$method, '_'), `[[`, 3) %>% unlist)
   dat_long_new <- dat_long %>% 
      mutate(algorithm=paste0(model, '_', scenario),
             index=ifelse(scenario%in%c('ratio', 'diff'), 2, 1),
             scenario=ifelse(grepl('20', scenario), 'single-year', scenario),
             algorithm=ifelse(index==2, algorithm, as.character(model)),
             model=ifelse(model%in%c('gwr', 'gtwr'), 'gwr/gtwr', as.character(model))
      ) 
   
   
   ggplot(dat_long_new)+
      geom_boxplot(aes(reorder(algorithm, index), values, 
                       fill=scenario))+
      facet_grid(year~model, scales='free_x')+
      labs(title=paste0(target_poll2),
           x='model', y='prediction')+
      theme_bw()+
      theme(strip.text = element_text(size = 12),
            axis.title = element_text(size = 19),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(size = 12, angle = 20),
            title = element_text(size = 19),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            panel.grid.major = element_line(colour = "grey70", size = 0.2),
            panel.grid.minor = element_blank())
   
   ggsave(paste0('graph/backExtrapo_boxplot_', target_poll, '.png'),
          height=8, width=8, units='in', dpi=100)
}
stopCluster(cl)
gc()



##########-------------- Correlation geographic maps -------------- ##########
cluster_no <- 4
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(poll_i = seq_along(target_polls))  %dopar%  {
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
   
   target_poll <- target_polls[poll_i]      #'NO2'    # PM25
   target_poll2 <- target_polls2[poll_i]    #'NO2'   # PM2.5
   no2 <- fread(paste0('data/processed/prediction_random_extrapoAll_', target_poll2)) %>% 
      as.data.frame()
   
   exc_names <- c('system.index', 'constant', 'latitude', 'longitude', 'x', '.geo',
                  'b1')
   
   no2_clean <- no2[, which(!(names(no2)%in%exc_names))]
   no2_clean <- no2_clean[, !grepl('08.10|09.11|10.12|08.12|06.12|12.19', names(no2_clean))]
   no2_clean <- cbind(no2_clean %>% dplyr::select('cntr_id', 'nuts_id'),
                      no2_clean %>% dplyr::select(-'cntr_id', -'nuts_id'))
   
   
   
   
   strs_ratio <- data.frame(str1=as.character(paste0('gtwr_00.19_', c(seq(2000, 2015, 5), 2019))),
                            str2=as.character(paste0('gtwr_ratio_', c(seq(2000, 2015, 5), 2019))))
   strs_diff <- data.frame(str1=as.character(paste0('gtwr_00.19_', c(seq(2000, 2015, 5), 2019))),
                            str2=as.character(paste0('gtwr_diff_', c(seq(2000, 2015, 5), 2019))))
   eu_bnd <- st_read("data/processed/nuts1_expanse.shp")
   eu_bnd2 <- st_read("../expanse_shp/eu_expanse2.shp")
   
   eu_bnd <- st_crop(eu_bnd, st_bbox(eu_bnd2))
   plotCorMap <- function(strs_v){
      r2tmapL <- lapply(1:nrow(strs_v), function(i){
         # > str1 <- 'gtwr_00.19_2000'
         # > str2 <- 'gtwr_diff_2000'
         str1 <- as.character(strs_v[i, 1])
         str2 <- as.character(strs_v[i, 2])
         strname1 <- paste0(strsplit(str1, '_')[[1]][1], '_', strsplit(str1, '_')[[1]][2])
         strname2 <- paste0(strsplit(str2, '_')[[1]][1], '_', strsplit(str2, '_')[[1]][2])
         yr <- strsplit(str2, '_')[[1]][3]
         
         if(!(grepl('2010', str1)&grepl('2010', str2))){
            cor_v <- lapply(unique(no2_clean$nuts_id), function(target_nuts){
               # > target_cntr='AL'
               target_df <- no2_clean[no2_clean$nuts_id==target_nuts, ]
               data.frame(cor=cor(target_df[, -c(1,2)])[str1, str2],
                          cntr_code=target_df$cntr_id[1],
                          nuts_id=target_df$nuts_id[1]) %>% 
                  mutate(r2=cor*cor) %>% 
                  rename(setNames('cor', paste0(target_poll2, ': cor(', strname1, ', ', strname2, ') in ', yr)),
                         setNames('r2', paste0(target_poll2, ': r2(', strname1, ', ', strname2, ') in ', yr)))
            }) %>% do.call(rbind, .)
            unique(eu_bnd$CNTR_CODE) %>% sort()
            names(cor_v) <- toupper(names(cor_v))
            cor_v_sp <- left_join(eu_bnd, cor_v, by=c('CNTR_CODE', 'NUTS_ID'))
            
            r2tmap <- tm_shape(cor_v_sp)+
               tm_polygons(col=names(cor_v_sp)[10],  ##COR
                           style = "fixed",
                           breaks = c(-1, seq(0, 1, 0.2)),
                           palette = c('#b44a46', "#FF0000", "#FFA500", '#FFFF00', "#8FBC8F", "#4682B4"))+
               tm_layout(title=paste0(target_poll2, ' ', yr),
                         legend.text.size = 0.6,
                         legend.show = F,
                         frame = T)
            # tm_shape(cor_v_sp)+
            #    tm_polygons(col=names(cor_v_sp)[10],  ##COR
            #                style = "fixed",
            #                breaks = seq(0.2, 1, 0.2),
            #                palette = c("#FF0000", "#FFA500", '#FFFF00', "#8FBC8F", "#4682B4"))+
            #    tm_layout(legend.outside = TRUE,
            #              legend.title.size = 1, legend.text.size = 0.6) 
            r2tmap
         }
         
      })
      r2tmapL <- r2tmapL[!unlist(lapply(r2tmapL, is.null))]
      r2tmap <- do.call(tmap_arrange, r2tmapL)
      strname2 <- strsplit(strs_v[1, 2], '_')[[1]][2]
      tmap_save(r2tmap, paste0('graph/backExtra_cntr_', target_poll2, '_', 
                               strname2, '.tiff'),
                dpi=150, height=4, width=14, units='in')  
   }
   plotCorMap(strs_ratio)
   plotCorMap(strs_diff)
   
}
stopCluster(cl)
gc()

