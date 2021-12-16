# This code is to solve the problem of exceeding user's memory in GEE 
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(doParallel)
library(foreach)
# Whether to tune RF

validation_str <- c('obsFinalModel')

tuneRF_b = T
local_crs <- CRS("+init=EPSG:3035")
seed <- 123
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")

csv_names <- lapply(validation_str, function(valid_str){
   target_pollv = c('NO2', 'PM2.5', 'PM10', 'O3')
   lapply(target_pollv, function(target_poll){
      paste0(valid_str, '_', target_poll, "_", c('00-19'))  
   }) %>% unlist
}) %>% unlist

years <- rep(list(2000:2019), length(csv_names))

# Check whether the predictors are processed or not

# Run the models for year 2010 at escape points and for every year at random points
cluster_no <- 5
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(yr_i = seq_along(csv_names))%dopar%{
   source("../EXPANSE_algorithm/scr/fun_call_lib.R")
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   source('../EXPANSE_algorithm/scr/fun_read_validation.R')
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   library(data.table)
   csv_name <- csv_names[yr_i]
   valid_str <- unlist(strsplit(csv_name, '_'))[1]
   target_poll <- unlist(strsplit(csv_name, '_'))[2]
   
   if((!file.exists(paste0('data/workingData/RF_result_validation_', csv_name, '.csv')))){ # &(!file.exists(paste0('data/workingData/RF_result_validation_', csv_name, '.csv')))
      df_all <- read_data(target_poll,  years[[yr_i]])
      target_df <- df_all
      pred_c <- select_predictor(df_all)
      
      if(nrow(target_df)>200){
         print("********************************************")
         print(csv_name)
         print(target_poll)
         
         ## Do the modelling for SLR and GWR in GEE and leave the RF modelling in R
         # --------- RF: split data into train, validation, and test data--------
         print("--------------- RF ---------------")
         set.seed(seed)
         # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
         # train_df <- data_all[index$train, ]
         # valid_df <- data_all[index$valid, ]
         # test_df <- data_all[index$test, ]
         train_df <- target_df
         pred_c_rf <- c(pred_c, 'year') #"x_trun", "y_trun"  ,  "cntr_code"
         x_varname = names(train_df %>% dplyr::select(pred_c_rf))
         print("RF predictors:")
         print(x_varname)
         hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", 
                                       gsub(valid_str, 'all', csv_name)
                                       ,".csv"))
         
         source("../EXPANSE_algorithm/scr/fun_output_rf_pred.R")
         # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
         rf_result <- output_rf_pred(train_df, train_df,
                                     y_varname='obs',
                                     x_varname = x_varname, hyper_grid, tuneRF_b)
         write.csv(rf_result, 
                   paste0('data/workingData/RF_result_validation_', csv_name, '.csv'), 
                   row.names = F)
      }
   }
}
stopCluster(cl)



