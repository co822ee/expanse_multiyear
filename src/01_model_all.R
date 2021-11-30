# This is a script to run the whole model and output results to GEE 
# need to rerun SLR for NO2 and PM2.5 (add year indicator in the final step)

source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
target_poll = 'PM10'
tuneRF_b = T
local_crs <- CRS("+init=EPSG:3035")
seed <- 123
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
csv_names <- paste0('all_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
source("../expanse_multiyear/src/00_fun_read_data_gee.R")
source('../EXPANSE_algorithm/scr/fun_select_predictor.R')

# Check whether the predictors are processed or not
run_i <- 1
for(yr_i in seq_along(csv_names)){
   df_all <- read_data(target_poll,  years[[yr_i]])
   target_df <- df_all
   pred_c <- select_predictor(df_all)
   
   # Don't need to do this because the sign of the predictor values is changed in GEE already
   # if(target_poll=='PM2.5'){
   #    neg_pred <- pred_c[grepl("clc14|clc7|precip", pred_c)]
   # }else if(target_poll=='NO2'){
   #    neg_pred <- pred_c[grepl("clc14|clc7|precip|wind", pred_c)]
   # }
   if(nrow(target_df)>200){
      
      csv_name <- csv_names[yr_i]
      print("********************************************")
      print(csv_name)
      print(target_poll)
      source("../EXPANSE_algorithm/scr/fun_slr_for.R")
      # check the predictor variables
      print("SLR predictors:")
      # source("../EXPANSE_algorithm/scr/fun_slr_proc_in_data.R")
      # target_df2 <- proc_in_data(target_df, neg_pred, "xcoord", "ycoord")
      target_df %>% dplyr::select(pred_c) %>% names()
      if(target_poll=='NO2'&any(years[[yr_i]]%in%2019)){
         target_df = target_df[-which.max(target_df$obs), ]
      }
      slr_result <- slr(target_df$obs, as.data.frame(target_df[, pred_c[pred_c!='year']]),
                        cv_n = csv_name,
                        R2thres = ifelse(target_poll=='PM2.5', 0.0, 0.01))
      slr_model <- slr_result[[3]]

      ### new: Add year for multiple year modelling
      slr_model <- lm(as.formula(paste0('obs~', paste(c(names(slr_model$coefficients[-1]), 'year'), collapse = '+'))),
                      data = target_df)
      slr_model %>% summary
      slr_output <- summary(slr_model)$coefficients %>% as.data.frame()
      slr_output <- cbind(row.names(slr_output), slr_output)
      slr_output[1,1] <- 'Final'
      colnames(slr_output) <- c("variables", "beta", "Std.Error", "t", "P")
      write.csv(slr_output, paste0('data/workingData/SLR_summary_model_year_', csv_name, '.csv'), row.names=F)

      #f# SLR: test SLR
      source("../EXPANSE_algorithm/scr/fun_gen_pred_df.R")
      slr_df <- gen_pred_df(slr_model, target_df, 'obs')[, c("sta_code", "slr", "obs", "res",
                                                              "year")]
      # --------- RF: split data into train, validation, and test data--------
      print("--------------- RF ---------------")
      set.seed(seed)
      # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
      # train_df <- data_all[index$train, ]
      # valid_df <- data_all[index$valid, ]
      # test_df <- data_all[index$test, ]
      train_df <- target_df
      pred_c_rf <- c(pred_c, "year", "zoneID") #"x_trun", "y_trun"
      x_varname = names(train_df %>% dplyr::select(pred_c_rf))
      print("RF predictors:")
      print(x_varname)
      ## LLO CV (small test for multiple years)

      if(tuneRF_b){
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
         hyper_grid <- tune_rf(train_df, train_df, #valid_df,
                               y_varname='obs',
                               x_varname,
                               csv_name, hyper_grid)

         #f# RF: train the model
         hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
      }
      source("../EXPANSE_algorithm/scr/fun_opt_rf.R")
      # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
      rf_result <- opt_rf(train_df, train_df,
                          y_varname='obs',
                          x_varname = x_varname,
                          csv_name, hyper_grid, tuneRF_b,
                          outputselect = c("sta_code", "rf", "obs", "res",
                                           "df_type", "year"))[[1]] %>% filter(df_type=='train') %>% dplyr::select(-df_type)

      source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
      plot_rf_vi(csv_name, var_no = 15)
      
      ### Output parameter outcome
      hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
      # hyper_grid2 <- hyper_grid[hyper_grid$ntree==min(hyper_grid$ntree),]
      slr_summary <- read.csv(paste0("data/workingData/SLR_summary_model_year_", csv_name, ".csv") )[, c('variables', 'beta')]
      slr_summary$poll <- target_poll
      hyper_grid$yr_str <- strsplit(csv_name, '_') %>% lapply(., `[[`, 3) %>% unlist
      slr_summary$csv_name = csv_name
      if(run_i==1){
         # gwrCoefAll <- coef_stack
         hyperAll <- hyper_grid[which.min(hyper_grid$OOB_RMSE),] %>% mutate( csv_name=csv_name)
         slrCoefAll <- slr_summary
      }else{
         # gwrCoefAll <- stack(gwrCoefAll, coef_stack)
         hyperAll <- rbind(hyperAll, hyper_grid[which.min(hyper_grid$OOB_RMSE),] %>% mutate(csv_name=csv_name))
         slrCoefAll <- rbind(slrCoefAll, slr_summary)
      }
      run_i <- run_i+1
   }
}

write.csv(hyperAll, paste0('data/processed/RFhyper_', 'all_', target_poll, "multipleyear.csv"), 
          row.names = F)
# write.csv(slrCoefAll, paste0('data/processed/SLRcoef_', 'all_', target_poll, "multipleyear.csv"), 
#           row.names = F)   ## with year

## combine air pollutants and create new variables in the table
rfFiles <- list.files('data/processed/', 'RFhyper_all_')
slrFiles <- list.files('data/processed/', 'SLRcoef_all_')

hyper <- lapply(paste0('data/processed/', rfFiles), function(fileName){
   hyper <- read.csv(fileName)
   hyper$poll <- strsplit(hyper$csv_name, '_') %>% lapply(., `[[`, 2) %>% unlist
   hyper$yr_str <- strsplit(hyper$csv_name, '_') %>% lapply(., `[[`, 3) %>% unlist
   
   start_yr <- paste0('20', strsplit( hyper$yr_str, '-') %>% lapply(., `[[`, 1) %>% unlist) %>% as.numeric
   end_yr <- paste0('20', strsplit( hyper$yr_str, '-') %>% lapply(., `[[`, 2) %>% unlist) %>% as.numeric
   yr_l <- lapply(seq_along(start_yr), function(i){
      (start_yr[i]):(end_yr[i])
   }) 
   inner_join(hyper, 
              data.frame(year=unlist(yr_l), 
                         yr_str=rep(hyper$yr_str, lapply(yr_l, length) %>% unlist())),
              by='yr_str')
   # hyper
}) %>% do.call(rbind,. )
# if(target_poll=='PM2.5'){
#    neg_pred <- pred_c[grepl("clc14|clc7|precip", pred_c)]
# }else if(target_poll=='NO2'){
#    neg_pred <- pred_c[grepl("clc14|clc7|precip|wind", pred_c)]
# }

slrCoefs <- lapply(paste0('data/processed/', slrFiles), function(fileName){
   ## The expansion of years for each multiple year model is done separately for year indicators and 
   ## other variables. 
   slrCoefs <- read.csv(fileName)
   slrCoefs$index <- 1:nrow(slrCoefs)
   slrCoefs$poll <- strsplit(slrCoefs$csv_name, '_') %>% lapply(., `[[`, 2) %>% unlist
   slrCoefs$yr_str <- strsplit(slrCoefs$csv_name, '_') %>% lapply(., `[[`, 3) %>% unlist
   # part 1 (other variables)
   
   slrCoefs1 <- slrCoefs %>% filter(!grepl('year', variables))  # without year indicator
   start_yr <- paste0('20', strsplit( slrCoefs1$yr_str, '-') %>% lapply(., `[[`, 1) %>% unlist) %>% as.numeric
   end_yr <- paste0('20', strsplit( slrCoefs1$yr_str, '-') %>% lapply(., `[[`, 2) %>% unlist) %>% as.numeric
   yr_l <- lapply(seq_along(start_yr), function(i){
      (start_yr[i]):(end_yr[i])
   })
   
   temp <- left_join(slrCoefs1, 
                     data.frame(year=unlist(yr_l), 
                                yr_str=rep(slrCoefs1$yr_str, lapply(yr_l, length) %>% unlist()),
                                variables=rep(slrCoefs1$variables, lapply(yr_l, length) %>% unlist())),
                     by=c('variables', 'yr_str'))
   # part 2 (only year indicators)
   slrCoefs2 <- slrCoefs %>% filter(grepl('year', variables)) %>% 
      mutate(year = as.numeric(gsub('year', '', variables))) %>% 
      rbind(., slrCoefs %>% filter(grepl('Final', variables)) %>% mutate(year=paste0('20', strsplit(yr_str, '-') %>% lapply(., `[[`, 1) %>% unlist) %>% as.numeric))
   slrCoefs2 <- slrCoefs2[order(slrCoefs2$index), ] %>% mutate(yr_str=as.factor(yr_str))
   
   # Combine the two
   slrCoefsAll <- rbind(temp %>% filter(variables=='Final'), 
                        slrCoefs2 %>% filter(variables!='Final')) %>% 
      arrange(index) %>% 
      mutate(start_yr = 
                paste0('20', strsplit(yr_str, '-') %>% lapply(., `[[`, 1) %>% unlist) %>% as.numeric)
   realFinal <- slrCoefsAll %>% filter(start_yr==year)
   baselineFinal <- slrCoefsAll %>% filter(start_yr!=year&variables=='Final')
   relativeFinal <- slrCoefsAll %>% filter(start_yr!=year&variables!='Final')
   trueFinal <- baselineFinal
   trueFinal$beta <- trueFinal$beta+relativeFinal$beta
   
   AllCoefs <- rbind(realFinal %>% dplyr::select(-start_yr), 
                     trueFinal %>% dplyr::select(-start_yr), 
                     temp %>% filter(variables!='Final')) %>% arrange(index)  ## AllCoefs and temp
   AllCoefs
   
}) %>% do.call(rbind,. )
if(!dir.exists('data/processed/combined/')) dir.create('data/processed/combined/')
write.csv(slrCoefs, 'data/processed/combined/SLRcoef_all_multiYears.csv', row.names=F)
write.csv(hyper, 'data/processed/combined/RFhyper_all_multiYears.csv', row.names=F)
