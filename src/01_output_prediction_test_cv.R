source("../EXPANSE_algorithm/scr/fun_call_lib.R")
source("src/00_fun_read_data_gee.R")
# Multiple single years
target_poll <- c('PM2.5', 'PM10', 'NO2', 'O3')
csv_names <- lapply(target_poll, function(poll_name){
   paste0('o3_', poll_name, "_",c('08-10', '09-11', '10-12', 
                                  '08-12', '06-12', '12-19', '00-19'))
}) %>% unlist   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
# list.files('data/workingData/', paste0('SLR_result_all_o2_', target_poll))

nfold=5

write_output_5csv <- function(year_i){
   print(csv_names[year_i])
   # paste0("GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv")
   # list.files("data/workingData/", paste0("GWR_result_all_", csv_names[year_i], "_fold"))

   slr <- lapply(paste0("data/workingData/SLR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"),
                 read.csv)
   print('gtwr')
   gtwr <- lapply(paste0('data/workingData/GTWR_result_all_', csv_names[year_i], "_fold_", seq(1,nfold), ".csv"),
                  read.csv)
   rf <- lapply(paste0("data/workingData/RF_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"),
                read.csv)
   
   slr_test <- lapply(slr, function(df_data) df_data %>% filter(df_type=='test'))
   gtwr_test <- gtwr ## It's already hold-out fold
   rf_test <- lapply(rf, function(df_data) df_data %>% filter(df_type=='test'))
   
   
   slr_test <- do.call(rbind, slr_test)
   # # nfold=4 PM2.5 from year 2000-2003 (00-19) there are NAs
   ## (because the error of optimizing the bandwidth size: error: inv(): matrix seems singular)
   gtwr_test <- do.call(rbind, gtwr_test)
   # slr_rf_test <- do.call(rbind, slr_rf_test)
   # gwr_test <- do.call(rbind, gwr_test)
   rf_test <- do.call(rbind, rf_test)

   all_test <- cbind(rf=rf_test$rf, # %>% select(rf, sta_code, year, index), 
                     gtwr=gtwr_test$gtwr[!is.na(gtwr_test$index)], #%>% select(gtwr, sta_code, year, index),
                     slr_test)
   write.csv(all_test, paste0("data/workingData/5cv_", csv_names[year_i], ".csv"), row.names = F)

}
lapply(seq_along(csv_names), write_output_5csv)

