
#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
target_polls = c('NO2', 'O3', 'PM10', 'PM2.5')
# Multiple single years
csv_names <- lapply(target_polls, function(target_poll){
   paste0('all_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                    '08-12', '06-12', '12-19', '00-19'))   #2008:2012
}) %>% unlist()
#--------- optimized parameters ------
# Save the optimized parameters as csv file
gtwr_param <- lapply(paste0('all_',c('NO2', 'O3', 'PM10', 'PM2.5'), '_00-19'), function(csv_name){
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', gsub('all_', 'o3_', csv_name)))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   cbind(gtwr_param[which.max(gtwr_param$rsq), ], 
         poll=strsplit(csv_name, '_')[[1]][2])
   
}) %>% do.call(rbind, .)
gtwr_param <- gtwr_param  %>% 
   dplyr::select(poll, conv_dist, lamda, ksi, rsq, ndata) %>% 
   rename(R2=rsq, 'conversion factor'=conv_dist,
          'scale factor'=lamda, 'interaction factor'=ksi,
          pollutant=poll) %>% 
   mutate(R2=round(R2, digits = 3))

write.csv(gtwr_param, 'data/processed/gtwr_opt_param.csv', row.names = F)
# Show the optimized values
do.call(rbind, lapply(paste0('all_', c('NO2', 'O3', 'PM10', 'PM2.5'), '_00-19'), function(csv_name){
   poll <- strsplit(csv_name, '_')[[1]][2]
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', gsub('all_', 'o3_', csv_name)))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   opt_lamda <- gtwr_param$lamda[which.max(gtwr_param$rsq)]
   opt_ksi <- gtwr_param$ksi[which.max(gtwr_param$rsq)]
   opt_convDist <- gtwr_param$conv_dist[which.max(gtwr_param$rsq)]
   data.frame(poll=poll, opt_lamda=opt_lamda, opt_ksi=opt_ksi, opt_convDist=opt_convDist)
}))

lapply(paste0('all_', c('NO2', 'O3', 'PM10', 'PM2.5'), '_00-19'), function(csv_name){
   poll <- strsplit(csv_name, '_')[[1]][2]
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', gsub('all_', 'o3_', csv_name)))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   
   
   opt_lamda <- gtwr_param$lamda[which.max(gtwr_param$rsq)]
   opt_ksi <- gtwr_param$ksi[which.max(gtwr_param$rsq)]
   opt_convDist <- gtwr_param$conv_dist[which.max(gtwr_param$rsq)]
   
   p1 <- ggplot(gtwr_param %>% filter(lamda==opt_lamda, ksi==opt_ksi))+
      geom_line(aes(conv_dist, rsq))+
      labs(title=poll)
   p2 <- ggplot(gtwr_param %>% filter(conv_dist==opt_convDist, ksi==opt_ksi))+
      geom_line(aes(lamda, rsq))
   p3 <- ggplot(gtwr_param %>% filter(conv_dist==opt_convDist, lamda==opt_lamda))+
      geom_line(aes(ksi, rsq))
   grid.arrange(p1, p2, p3)
}) %>% do.call(grid.arrange, .)

opt_param <- lapply(csv_names, function(csv_name){
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', gsub('all_', 'o3_', csv_name)))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   # gtwr_param2 <- gtwr_param %>% mutate(alpha=(1-lamda)*conv_dist, beta=lamda*alpha)
   opt_param <- gtwr_param[which.max(gtwr_param$rsq), ]
   opt_param$poll <- strsplit(csv_name, '_')[[1]][2]
   opt_param$period <- strsplit(csv_name, '_')[[1]][3]
   opt_param
}) %>% do.call(rbind, .)

opt_param <- opt_param %>% mutate(alpha=(1-lamda)*conv_dist, beta=lamda*alpha)

write.csv(opt_param %>% filter(period=='00-19'),
          'results/output/table_GTWR_opt_param.csv')

