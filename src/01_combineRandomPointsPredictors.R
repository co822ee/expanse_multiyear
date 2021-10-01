library(dplyr)
lapply(c('no2', 'o3', 'pm25', 'pm10'), function(poll_str){
   df_random <- lapply(paste0('..//EXPANSE_predictor/data/raw/gee/', 
                              list.files('..//EXPANSE_predictor/data/raw/gee/', 
                                         paste0('pred_', poll_str, '_random'))), 
                       read.csv) %>% do.call(rbind, .)
   write.csv(df_random, paste0('../EXPANSE_predictor/data/raw/gee/pred_', poll_str, '_random.csv'))
})