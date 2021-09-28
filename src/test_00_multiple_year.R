# This script run the three models for multiple single years or multiple years
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
source("src/00_fun_read_data.R")
# Whether to tune RF
tuneRF_b = F
# Multiple single years
csv_names <- paste0('o_', "06-10")   #2008:2012
years <- list(c(2006:2010))
yr_i <- 1
csv_name <- csv_names[yr_i]
print("********************************************")
print(csv_name)
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
ex_var <- c("id", "sta_code", "cntr_code", "country_name", 
            "sta_type", "area_type", "areaid", "xcoord", "ycoord",
            "zoneID", "obs", "component_code", "component_caption", "year")
pred_c <- names(no2_e_09_11)[!(names(no2_e_09_11)%in%ex_var)]
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
# numbers of observations for every year
yrs <- unique(no2_e_09_11$year)
poll_tbl <- with(no2_e_09_11, table(sta_code, year))
temporal_avail <- apply(poll_tbl, 1, sum)
str(temporal_avail)
temporal_avail <- data.frame(sta_code=names(temporal_avail),
                             n_obs=as.numeric(temporal_avail))
temporal_avail <- inner_join(temporal_avail,
                             no2_e_09_11 %>% dplyr::select(sta_code, sta_type, zoneID), 
                             by="sta_code")
temporal_avail <- temporal_avail[!duplicated(temporal_avail$sta_code),]

# All year available
# temporal_avail[temporal_avail==length(yrs)] %>% names
source("src/00_fun_create_fold.R")
# The stations will only be included in one specific fold.
sta_split <- create_fold(temporal_avail, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                         nfold = 5)
data_all1 <- inner_join(no2_e_09_11, sta_split %>% dplyr::select(sta_code, nfold, n_obs), by="sta_code")
data_all1$index <- 1:nrow(data_all1)
fold_i=1
csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
test_sub <- data_all1[data_all1$nfold==fold_i,]
train_sub <- data_all1[-test_sub$index, ] #data_all1$index starts from 1 to the length.
# Test whether The stations will only be included in one specific fold.
any(test_sub$sta_code%in%train_sub$sta_code)

#f# SLR: select predictors
source("../EXPANSE_algorithm/scr/o_00_01_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("../EXPANSE_algorithm/scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred, "xcoord", "ycoord")
test_sub <- proc_in_data(test_sub, neg_pred, "xcoord", "ycoord")
data_all <- rbind(train_sub, test_sub)
#------------------Above code is needed for all algorithms----------------------
#---------#f# SLR: train SLR -----------
source("../EXPANSE_algorithm/scr/fun_slr_for.R")
# check the predictor variables
print("SLR predictors:")
x_var <- train_sub %>% dplyr::select(matches(pred_c), "year", "zoneID") %>% names()
slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(all_of(x_var)) %>% as.data.frame(),
                  cv_n = csv_name_fold)
slr_model <- slr_result[[3]]
slr_model %>% summary
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
#--------- RF: split data into train, validation, and test data--------
print("--------------- RF ---------------")
set.seed(seed)
# index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
# train_df <- data_all[index$train, ]
# valid_df <- data_all[index$valid, ]
# test_df <- data_all[index$test, ]
train_df <- train_sub
test_df <- test_sub
pred_c_rf <- c(pred_c, "year", "zoneID") #"x_trun", "y_trun"  ,  "cntr_code"
x_varname = names(data_all %>% dplyr::select(pred_c_rf))
print("RF predictors:")
print(x_varname)
## LLO CV (small test for multiple years)

if(tuneRF_b){
   #f# RF: tune hyperparameter
   hyper_grid <- expand.grid(
      mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
      ntrees = seq(500,1000, by=200),
      OOB_RMSE = 0,
      OOB_R2 = 0,
      valid_RMSE = 0,
      valid_R2 = 0
   )
   source("scr/fun_tune_rf.R")
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
                    csv_name_fold, hyper_grid, tuneRF_b,
                    outputselect = c("sta_code", "rf", "obs", "res",
                                     "nfold", "df_type", "year", "index"))
source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
plot_rf_vi(csv_name_fold, var_no = 10)
# Model Performance evaluation:
slr_poll$eval_train %>% print()
slr_poll$eval_test %>% print()
rf_result$eval_train
rf_result$eval_test

# sta_split %>% names()
# (sta_split  %>% dplyr::filter(n_obs==5, sta_code=="AT30101") %>% dplyr::select(nfold, n_obs))
# (sta_split %>% dplyr::filter(nfold==1) %>% dplyr::filter(n_obs==5) %>% nrow)/(data_all1 %>% nrow)
# (sta_split %>% dplyr::filter(nfold==2) %>% dplyr::filter(n_obs==5) %>% nrow)/(data_all1 %>% nrow)
# (sta_split %>% dplyr::filter(nfold==1) %>% dplyr::filter(n_obs==4) %>% nrow)/(data_all1 %>% nrow)
# (sta_split %>% dplyr::filter(nfold==2) %>% dplyr::filter(n_obs==4) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==1) %>% dplyr::filter(n_obs==5, year==2006) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==2) %>% dplyr::filter(n_obs==5, year==2006) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==1) %>% dplyr::filter(n_obs==4, year==2006) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==2) %>% dplyr::filter(n_obs==4, year==2006) %>% nrow)/(data_all1 %>% nrow)


