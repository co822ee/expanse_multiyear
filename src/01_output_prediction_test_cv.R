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
   # slr_rf <- lapply(paste0("data/workingData/slr_rf_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"),
   #                  read.csv)
   # gwr <- lapply(paste0("data/workingData/GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"),
   #               read.csv)
   rf <- lapply(paste0("data/workingData/RF_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"),
                read.csv)
   
   slr_test <- lapply(slr, function(df_data) df_data %>% filter(df_type=='test'))
   # slr_rf_test <- lapply(slr_rf, function(df_data) df_data %>% filter(df_type=='test'))
   # gwr_test <- lapply(gwr, function(df_data) df_data %>% filter(df_type=='test'))
   rf_test <- lapply(rf, function(df_data) df_data %>% filter(df_type=='test'))
   # gtwr_test <- lapply(paste0('data/workingData/gtwr_pred_', csv_names[year_i], '_fold_', seq(1, nfold), '.csv'),
   #                     read.csv) %>% do.call(rbind,.)
   
   slr_test <- do.call(rbind, slr_test)
   # slr_rf_test <- do.call(rbind, slr_rf_test)
   # gwr_test <- do.call(rbind, gwr_test)
   rf_test <- do.call(rbind, rf_test)

   all_test <- cbind(rf=rf_test$rf, 
                     # gtwr=gtwr_test$gtwr, 
                     slr_test)  #gwr=gwr_test$gwr  #slr_rf=slr_rf_test$slr_rf,
   write.csv(all_test, paste0("data/workingData/5cv_", csv_names[year_i], ".csv"), row.names = F)

}
lapply(seq_along(csv_names), write_output_5csv)


# Another way (never used)
# paste0("GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv")
# list.files("data/workingData/", paste0("GWR_result_all_", csv_names[year_i], "_fold"))
# slr <- lapply(paste0("data/workingData/", 
#                      list.files("data/workingData/", "SLR_result_all_")), 
#               read.csv)
# # gwr <- lapply(paste0("data/workingData/GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
# #               read.csv)
# rf <- lapply(paste0("data/workingData/", 
#                     list.files("data/workingData/", "RF_result_all_")), 
#              read.csv)
# 
# slr_test <- lapply(slr, function(df_data) df_data %>% filter(df_type=='test'))
# # gwr_test <- lapply(gwr, function(df_data) df_data %>% filter(df_type=='test'))
# rf_test <- lapply(rf, function(df_data) df_data %>% filter(df_type=='test'))
# 
# slr_test <- do.call(rbind, slr_test)
# # gwr_test <- do.call(rbind, gwr_test)
# rf_test <- do.call(rbind, rf_test)
# 
# all_test <- cbind(rf=rf_test$rf, slr_test)  #gwr=gwr_test$gwr
# write.csv(all_test, paste0("data/workingData/NO2_5cv_", csv_names[year_i], ".csv"))


 #---- test_grid----
regression_grd_cellsize <- c(10, 20, 50, 80, 100, 200, 500, 600, 1000, 1500, 2000)   #km
kernels <- c('exponential')
year_target <- 2010
nfolds=5
comb <- expand.grid(regression_grd_cellsize=regression_grd_cellsize, kernel_type=kernels, nfold=1:nfolds) %>% 
   mutate(csv_name = paste0('testGWR_', regression_grd_cellsize, '_', kernel_type, '_', year_target, "_fold_", nfold))
kernel_type <- comb$kernel_type %>% as.character()
reg_grdsize <- comb$regression_grd_cellsize*1000
csv_names <- comb$csv_name
write_output_5csv_test_grid <- function(file_i){
   # paste0("GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv")
   # list.files("data/workingData/", paste0("GWR_result_all_", csv_names[year_i], "_fold"))

   gwr <- lapply(paste0("data/workingData/", unique(substr(csv_names, 1, nchar(csv_names)-7))[file_i], "_fold_", seq(1,nfolds), ".csv"), 
                 read.csv)
   
   gwr_test <- do.call(rbind, gwr)

   write.csv(gwr_test, paste0("data/workingData/NO2_5cv_", 
                              unique(substr(csv_names, 1, nchar(csv_names)-7))[file_i], 
                              ".csv"))
   
}
lapply(seq_along(unique(substr(csv_names, 1, nchar(csv_names)-7))), write_output_5csv_test_grid)
