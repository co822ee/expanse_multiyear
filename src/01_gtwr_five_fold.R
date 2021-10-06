# This script runs the 5-fold CV for GTWR

#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(doParallel)
library(foreach)
target_poll = 'NO2'
obs_varname = 'obs'
# Multiple single years
csv_names <- paste0('o3_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
nfold <- 5
yr_i <- 7
# gtwr_yr <- 2010
# fold_i <- 1
## ------ *** useful function -----


for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   #---------- read in data ------
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   df_sub <- read_data(target_poll, years[[yr_i]])
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   pred_c <- select_predictor(df_sub)
   #--------- optimized parameters ------
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', csv_name))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   
   #------ 2) Subset training and test for 5-CV (this should be the same as slr and rf) ------
   source("src/00_fun_create_fold.R")
   # The stations will only be included in one specific fold.
   data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                           multiyear_group = c("sta_code", "year"),
                           nfold = 5)
   
   cluster_no <- 5
   cl <- parallel::makeCluster(cluster_no)
   doParallel::registerDoParallel(cl)
   foreach(fold_i = 1:nfold)  %dopar%  {
      source("../EXPANSE_algorithm/scr/fun_call_lib.R")
      source('src/fun_creat_spPoints.R')
      source('src/fun_create_regressionGrid.R')
      source('src/fun_gen_df_gtwr.R')
      source('src/fun_tuneGTWR.R')
      
      csv_name_fold <- paste0(csv_name, '_fold_', fold_i)
      test_sub <- data_all[data_all$nfold==fold_i,]
      train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
      
      sp_train <- creat_spPoints(train_sub, 'xcoord', 'ycoord', local_crs)
      sp_test <- creat_spPoints(test_sub, 'xcoord', 'ycoord', local_crs)
      
      #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
      slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name_fold, '.csv'))
      eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
      
      grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
      
      # Output the predictions at validation points using GTWR 
      gtwr_valid <- lapply(years[[yr_i]], tuneGTWR, sp_train1=sp_train, grid_i=which.max(gtwr_param$rsq),
                           sp_valid1=sp_test, valid_sub1=test_sub, 
                           target_poll=target_poll, eq=eq, grd=grd2, 
                           param=gtwr_param) %>% do.call(rbind, .) 
      names(gtwr_valid)[!names(gtwr_valid)%in%pred_c]
      gtwr_valid <- gtwr_valid %>% dplyr::select(!pred_c, 'year', -c('system.index', '.geo', 'ycoord','xcoord')) %>% names
      write.csv(gtwr_valid , paste0('data/workingData/GTWR_result_all_', csv_name_fold, '.csv'), row.names = F)
      
   }
   stopCluster(cl)
}