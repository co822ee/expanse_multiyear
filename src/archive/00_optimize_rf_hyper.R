# Not finished (because we used OOB_RMSE for tuning the hyperparameters instead)
# This tunes the hyperparameters in the random forests 

source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# source("src/00_fun_read_data.R")
source("src/00_fun_read_data_gee.R")
# Whether to tune RF
tuneRF = T
# Multiple single years
target_poll_v <- c('PM2.5', 'PM10', 'NO2', 'O3')
csv_names <- lapply(target_poll_v, function(poll_name){
   paste0('o3_', poll_name, "_",c('08-10', '09-11', '10-12', 
                                  '08-12', '06-12', '12-19', '00-19'))
}) %>% unlist   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
library(doParallel)
library(foreach)
nfold <- 5

cluster_no <- 5
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(yr_i = seq_along(csv_names))  %dopar%  {
   source('../EXPANSE_algorithm/scr/fun_call_lib.R')
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   target_poll <- unlist(strsplit(csv_name, '_'))[2]
   df_sub <- read_data(target_poll,  years[[yr_i]])
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   pred_c <- select_predictor(df_sub)
   
   print(paste0("year: ", unique(df_sub$year)))
   if(nrow(df_sub)>200){
      source("src/00_fun_create_fold.R")
      # The stations will only be included in one specific fold.
      data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                              multiyear_group = c("sta_code", "year"),
                              nfold = nfold)
      pred_c_rf <- c(pred_c, "year", "zoneID") #"x_trun", "y_trun"
      x_varname = names(data_all %>% dplyr::select(pred_c_rf))
      
      print("RF predictors:")
      print(x_varname)
      
      #f# RF: tune hyperparameter
      hyper_grid <- expand.grid(
         mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
         ntrees = seq(200,800, by=200),
         OOB_RMSE = 0,
         OOB_R2 = 0,
         valid_RMSE = 0,
         valid_R2 = 0
      )
      lapply(1:nrow(hyper_grid), function(grid_i){
         # Run the random forests using the specified hyperparmaeter
         validation <- lapply(1:nfold, function(fold_i){
            csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
            test_sub <- data_all[data_all$nfold==fold_i,]
            train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
            
            #--------- RF: split data into train, validation, and test data--------
            print("--------------- RF ---------------")
            set.seed(seed)
            train_df <- train_sub
            test_df <- test_sub
            
            
            source("../EXPANSE_algorithm/scr/fun_tune_rf2.R")
            hyper_grid <- tune_rf2(train_df, test_df, #valid_df,
                                  y_varname='obs',
                                  x_varname,
                                  csv_name_fold, hyper_grid, grid_i)
            hyper_grid
         }) %>% do.call(rbind, .)
      })
      
      
      write.csv(hyper_grid, paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
      
   }
      
}
stopCluster(cl)