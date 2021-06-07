# This script run the three models for multiple single years or multiple years
# 07052021 Run the code and include zoneID and cntr_code and year for SLR
# 03062021 Run the code and include zoneID as a predictor and without tuning RF
target_poll <- 'NO2'
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
source("src/00_fun_read_data.R")
# Whether to tune RF
tuneRF = F
# Multiple single years

# Multiple years
csv_names <- paste0('o2_',target_poll, "_",c('08-10', '09-11', '10-12', 
                              '08-12', '06-12', '12-18', '00-18'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2018, 2000:2018)
library(doParallel)
library(foreach)

for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   no2_e_all <- read_APdata(target_poll)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]], target_poll)
   exc_names <- c("sta_code", "component_code", "component_caption", "obs", 
                  "id", "country_name", "sta_type", "area_type", "areaid", 
                  "index", "nfold", "xcoord", "ycoord") #'cntr_code','zoneID'
   
   pred_c <- names(no2_e_09_11)[!(names(no2_e_09_11)%in%exc_names)]
   # data_all <- no2_e_09_11
   print(paste0("year: ", unique(no2_e_09_11$year)))
   if(nrow(no2_e_09_11)>200){
      source("src/00_fun_create_fold.R")
      # The stations will only be included in one specific fold.
      data_all1 <- create_fold(no2_e_09_11, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                               multiyear_group = c("sta_code", "year"),
                               nfold = 5)
      
      
      cluster_no <- 5
      cl <- parallel::makeCluster(cluster_no)
      doParallel::registerDoParallel(cl)
      foreach(fold_i = seq_along(unique(data_all1$nfold)))  %dopar%  {
         
         source('../EXPANSE_algorithm/scr/fun_call_lib.R')
         csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
         
         #f# SLR: select predictors
         # source("../EXPANSE_algorithm/scr/o_00_01_call_predictor.R")
         neg_pred <- pred_c[grepl("nat|ugr", pred_c)]
         #f# SLR: define/preprocess predictors (direction of effect)
         source("../EXPANSE_algorithm/scr/fun_slr_proc_in_data.R")
         data_all <- proc_in_data(data_all1, neg_pred, "xcoord", "ycoord")
         if(target_poll=='PM2.5') data_all <- data_all[!is.na(data_all$sat_pm25), ]
         test_sub <- data_all[data_all$nfold==fold_i,]
         train_sub <- data_all[-test_sub$index, ] #data_all1$index starts from 1 to the length.
         
         #------------------Above code is needed for all algorithms----------------------
         #---------#f# SLR: train SLR -----------
         source("../EXPANSE_algorithm/scr/fun_slr_for.R")
         # check the predictor variables
         print("SLR predictors:")
         x_var <- train_sub %>% dplyr::select(matches(pred_c), "year", "zoneID") %>% names()
         slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(x_var) %>% as.data.frame(),
                           cv_n = csv_name_fold, R2thres = ifelse(target_poll=='PM2.5', 0.0, 0.01))
         slr_model <- slr_result[[3]]
         # train_sub$year <- as.numeric(train_sub$year)
         # lm(as.formula(paste0('obs~', paste(names(coefficients(slr_model))[-1], collapse = '+'), '+year')), train_sub) %>% summary
         # lm(as.formula(paste0('obs~', paste(names(coefficients(slr_model))[-1], collapse = '+'), '+year')), train_sub) %>% summary
         #f# SLR: test SLR
         # source("../EXPANSE_algorithm/scr/fun_output_slr_result.R")
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
         # Force adding other predictors
         # eq_slr <- paste0("obs~", paste(c(names(slr_model$coefficients)[-1],
         #                                  "omi", "zoneID", "year"), collapse = "+")) %>% as.formula()
         # 
         # slr_2 <- lm(eq_slr, train_sub)  # exclude OMI because the coefficient is negative
         # eq_slr <- paste0("obs~", paste(c(names(slr_model$coefficients)[-1],
         #                                  "zoneID", "year"), collapse = "+")) %>% as.formula()
         # 
         # slr_2 <- lm(eq_slr, train_sub)  # exclude OMI because the coefficient is negative
         # data_all2 <- rbind(train_sub %>% mutate(df_type='train'), 
         #                    test_sub %>% mutate(df_type='test'))
         # poll2 <- gen_pred_df(slr_2, data_all2, "obs")
         # error_matrix(poll2[poll2$df_type=="train", "obs"], poll2$slr[poll2$df_type=="train"])
         # error_matrix(poll2[poll2$df_type=="test", "obs"], poll2$slr[poll2$df_type=="test"])
         # 
         # #f# SLR: predict residual using omi
         # slr_df2 <- inner_join(slr_df, data_all, by=c("station_european_code", "year", "obs", "nfold", "index"))
         # # Method 1: lm
         # lm_res <- lm(as.formula(paste0("res~omi+zoneID+year")), slr_df2[slr_df2$df_type=="train", ])
         # lm_res %>% summary
         # slr_poll2 <- data.frame(slr = (predict(lm_res, slr_df2) %>% as.numeric())+slr_df2$slr,
         #                         obs = slr_df2[, "obs"]) %>% 
         #    mutate(res = obs - slr) %>% 
         #    cbind(slr_df2 %>% dplyr::select(-all_of("obs")))
         # 
         # error_matrix(slr_poll2[slr_poll2$df_type=="train", "obs"], slr_poll2$slr[slr_poll2$df_type=="train"])
         # error_matrix(slr_poll2[slr_poll2$df_type=="test", "obs"], slr_poll2$slr[slr_poll2$df_type=="test"])
         # Method 2: RF
         # rf_res <- ranger(
         #    formula = as.formula(paste0("res~", paste(x_var, collapse = "+"))),
         #    data = slr_df2[slr_df2$df_type=="train", ],
         #    num.trees = 500,
         #    seed = seed,
         #    importance = 'impurity'          # 'permutation'
         # )
         # 
         # slr_rf_result <- data.frame(slr_rf = (predict(rf_res, slr_df2) %>% predictions()) + slr_df2$slr,
         #                             obs = slr_df2[, "obs"]) %>% 
         #    mutate(res = obs - slr_rf) %>% 
         #    cbind(slr_df2 %>% dplyr::select(-all_of("obs"))) 
         # write.csv(slr_rf_result, 
         #           paste0("data/workingData/slr_rf_result_all_", csv_name_fold, ".csv"),
         #           row.names = F)
         # 
         # error_matrix(slr_rf_result[slr_rf_result$df_type=="train", "obs"], slr_rf_result$slr_rf[slr_rf_result$df_type=="train"])
         # error_matrix(slr_rf_result[slr_rf_result$df_type=="test", "obs"], slr_rf_result$slr_rf[slr_rf_result$df_type=="test"])
         # 
         # var_importance <- data.frame(var_name = rf_res$variable.importance %>% names, 
         #                              vi = rf_res$variable.importance %>% as.numeric())
         # var_importance <- var_importance[with(var_importance, order(-vi)), ]
         # ggplot(var_importance %>% top_n(20, vi))+
         #    geom_col(aes(reorder(var_name, vi), vi),
         #             position = 'dodge', fill='khaki')+
         #    coord_flip() +
         #    theme_light()+
         #    labs(x = 'variable', y = 'importance value (impurity)',
         #         title = csv_name)+
         #    theme(axis.title = element_text(size = 13),
         #          axis.text = element_text(size = 13),
         #          legend.title = element_text(size = 13),
         #          legend.text = element_text(size = 13),
         #          strip.text.y = element_text(size = 12))
         # write.csv(var_importance, 
         #           paste0("data/workingData/slr_rf_vi_", csv_name_fold, ".csv"), 
         #           row.names = F)
         #-----------#f# GWR: train GWR--------
         #--------- RF: split data into train, validation, and test data--------
         print("--------------- RF ---------------")
         set.seed(seed)
         # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
         # train_df <- data_all[index$train, ]
         # valid_df <- data_all[index$valid, ]
         # test_df <- data_all[index$test, ]
         train_df <- train_sub
         test_df <- test_sub
         pred_c_rf <- c(pred_c, "year", "zoneID", 'cntr_code') #"x_trun", "y_trun"
         x_varname = names(data_all %>% dplyr::select(matches(pred_c_rf)))
         print("RF predictors:")
         print(x_varname)
         ## LLO CV (small test for multiple years)
         
         if(tuneRF){
            #f# RF: tune hyperparameter
            hyper_grid <- expand.grid(
               mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
               ntrees = seq(500,1000, by=200),
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

