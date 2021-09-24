# This script run the three models for multiple single years or multiple years
# 07052021 Run the code and include zoneID and cntr_code and year for SLR
# 03062021 Run the code and include zoneID as a predictor and without tuning RF
# 20062021 Run the code and include zoneID as a predictor and tune RF (o3_) (Overnight 06/20 NO2)
# Need to rerun SLR for NO2 and PM2.5 (add year indicator in the final step.)
# 2021/07/21 rerun SLR for NO2 and PM2.5 (add year indicator in the final step)
## note: SLR_summary_model_year_ gives the one after adding year indicator at the final step
# need to check the results: NO2, PM2.5


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
## PM10 6:7 unfinished
for(yr_i in seq_along(csv_names)){
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
                               nfold = 5)
      
      
      cluster_no <- 5
      cl <- parallel::makeCluster(cluster_no)
      doParallel::registerDoParallel(cl)
      foreach(fold_i = seq_along(unique(data_all$nfold)))  %dopar%  {
         source('../EXPANSE_algorithm/scr/fun_call_lib.R')
         csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
         test_sub <- data_all[data_all$nfold==fold_i,]
         train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
         
         #------------------Above code is needed for all algorithms----------------------
         #---------#f# SLR: train SLR -----------
         source("../EXPANSE_algorithm/scr/fun_slr_for.R")
         # check the predictor variables
         print("SLR predictors:")

         slr_result <- slr(train_sub$obs, as.data.frame(train_sub[, pred_c[pred_c!='year']]),
                           cv_n = csv_name_fold, R2thres = ifelse(target_poll=='PM2.5', 0.0, 0.01))
         slr_model <- slr_result[[3]]
         ### new: Add year for multiple year modelling
         slr_model <- lm(as.formula(paste0('obs~', paste(c(names(slr_model$coefficients[-1]), 'year'), collapse = '+'))),
                         data = train_sub)
         slr_model %>% summary
         slr_output <- summary(slr_model)$coefficients %>% as.data.frame()
         slr_output <- cbind(row.names(slr_output), slr_output)
         slr_output[1,1] <- 'Final'
         colnames(slr_output) <- c("variables", "beta", "Std.Error", "t", "P")
         write.csv(slr_output, paste0('data/workingData/SLR_summary_model_year_', csv_name_fold, '.csv'), row.names=F)

         source("../EXPANSE_algorithm/scr/fun_gen_pred_df.R")
         output_slr_result <- function(model, test_df, train_df, output_filename, obs_varname,
                                       outputselect = c("station_european_code", "slr", "obs", "res",
                                                        "nfold", "df_type", "year", "index")){
            slr_poll_test <- gen_pred_df(model, test_df, obs_varname)
            slr_poll_train <- gen_pred_df(model, train_df, obs_varname)
            eval_test <- error_matrix(slr_poll_test[, obs_varname], slr_poll_test$slr)
            eval_train <- error_matrix(slr_poll_train[, obs_varname], slr_poll_train$slr)

            slr_poll <- rbind(slr_poll_train %>% mutate(df_type = 'train'),
                              slr_poll_test %>% mutate(df_type = 'test'))
            slr_poll <- slr_poll[, outputselect]
            write.csv(slr_poll,
                      paste0('data/workingData/SLR_result_all_', output_filename, '.csv'),
                      row.names = F)
            return(list(slr_poll, eval_train=eval_train, eval_test=eval_test))
         }

         slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                                       output_filename = csv_name_fold, obs_varname = 'obs',
                                       outputselect = c("sta_code", "slr", "obs", "res",
                                                        "nfold", "df_type", "year", "index"))
         slr_df <- slr_poll[[1]]

         slr_poll$eval_train %>% print()
         slr_poll$eval_test %>% print()
         
         #--------- RF: split data into train, validation, and test data--------
         print("--------------- RF ---------------")
         set.seed(seed)
         # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
         # train_df <- data_all[index$train, ]
         # valid_df <- data_all[index$valid, ]
         # test_df <- data_all[index$test, ]
         train_df <- train_sub
         test_df <- test_sub
         pred_c_rf <- c(pred_c, "year", "zoneID") #"x_trun", "y_trun"
         x_varname = names(data_all %>% dplyr::select(pred_c_rf))
         print("RF predictors:")
         print(x_varname)
         ## LLO CV (small test for multiple years)
         
         if(tuneRF){
            #f# RF: tune hyperparameter
            hyper_grid <- expand.grid(
               mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
               ntrees = seq(200,800, by=200),
               OOB_RMSE = 0,
               OOB_R2 = 0,
               valid_RMSE = 0,
               valid_R2 = 0
            )
            source("../EXPANSE_algorithm/scr/fun_tune_rf.R")
            hyper_grid <- tune_rf(train_df, test_df, #valid_df,
                                  y_varname='obs',
                                  x_varname,
                                  csv_name_fold, hyper_grid)
            
            #f# RF: train the model
            hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name_fold,".csv"))
         }
         source("../EXPANSE_algorithm/scr/fun_opt_rf.R")
         # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
         rf_result <- opt_rf(train_df, test_df,
                             y_varname='obs',
                             x_varname = x_varname,
                             csv_name_fold, hyper_grid, tuneRF,
                             outputselect = c("sta_code", "rf", "obs", "res",
                                              "nfold", "df_type", "year", "index"))
         source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
         plot_rf_vi(csv_name_fold, var_no = 20)
         # Model Performance evaluation:
         slr_poll$eval_train %>% print()
         slr_poll$eval_test %>% print()
         # error_matrix(gwr_df[gwr_df$df_type=='train', 'obs'], gwr_df[gwr_df$df_type=='train', 'gwr']) %>% 
         #    print()
         # error_matrix(gwr_df[gwr_df$df_type=='test', 'obs'], gwr_df[gwr_df$df_type=='test', 'gwr']) %>% 
         #    print()
         rf_result$eval_train
         rf_result$eval_test
         # rf_result$rf_result %>% names
         # # output all models' performance matrix
         output_em <- function(pred_df, csv_name, model, year, obs_name){
            em <- rbind(error_matrix(pred_df[pred_df$df_type=='test', 'obs'], pred_df[pred_df$df_type=='test', model]) ,
                        error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', model])) %>%
               as.data.frame()
            # em[, c(1, 5, 7)]
            
            perf_matrix <- em %>% mutate(df_type=c('test','train'), model=model, n_fold=fold_i,
                                         csv_name=csv_name)
            perf_matrix
         }
         out_pm <- rbind(output_em(slr_df, csv_name_fold, 'slr', years[[yr_i]], "obs"),
                         # output_em(gwr_df, csv_name_fold, 'gwr', years[[yr_i]], "obs"),
                         output_em(rf_result$rf_result, csv_name_fold, 'rf', years[[yr_i]], "obs")
                         # output_em(slr_rf_result, csv_name_fold, 'slr_rf', years[[yr_i]], "obs")
         )
         write.csv(out_pm, paste0("data/workingData/perf_m_",csv_name_fold, '.csv'), row.names = F)
      }
      stopCluster(cl)
   }else{
      print('not enough data')
   }
} 
  
