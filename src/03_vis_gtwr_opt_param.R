
#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
target_polls = c('NO2', 'O3', 'PM10', 'PM2.5')
# Multiple single years
csv_names <- lapply(target_polls, function(target_poll){
   paste0('all_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                    '08-12', '06-12', '12-19', '00-19'))   #2008:2012
}) %>% unlist()
#--------- optimized parameters ------
opt_param <- lapply(csv_names, function(csv_name){
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', gsub('all_', 'o3_', csv_name)))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   opt_param <- gtwr_param[which.max(gtwr_param$rsq), ]
   opt_param$poll <- strsplit(csv_name, '_')[[1]][2]
   opt_param$period <- strsplit(csv_name, '_')[[1]][3]
   opt_param
}) %>% do.call(rbind, .)

opt_param <- opt_param %>% mutate(alpha=(1-lamda)*conv_dist, beta=lamda*alpha)

write.csv(opt_param %>% filter(period=='00-19'),
          'results/output/table_GTWR_opt_param.csv')

