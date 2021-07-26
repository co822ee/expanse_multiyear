## Done: run for NO2 and PM2.5
library(docstring)
#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(doParallel)
library(foreach)
target_poll = 'PM2.5'
obs_varname = 'obs'
nfold <- 5
# Multiple single years
csv_names <- paste0('o3_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
#-----useful function------
source('src/fun_creat_spPoints.R')
source('src/fun_gen_df_gtwr.R')
source('src/fun_trainGTWR.R')
source('src/fun_create_regressionGrid.R')

# five fold CV for GTWR
for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   
   csv_name <- csv_names[yr_i]
   df_sub <- read_data(target_poll, years[[yr_i]])
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
      test_sub <- data_all[data_all$nfold==fold_i,]
      train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
      
      sp_train <- creat_spPoints(train_sub, 'xcoord', 'ycoord', local_crs)
      sp_test <- creat_spPoints(test_sub, 'xcoord', 'ycoord', local_crs)
      
      #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
      slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '_fold_', fold_i, '.csv'))
      eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
      
      grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
      
      # Optimize the bandwidth for every year
      # Train the GTWR for every year
      # Get the predictions for the test data
      pred_test <- lapply(years[[yr_i]], trainGTWR, sp_train1=sp_train, 
                          sp_valid1=sp_test, valid_sub1=test_sub, 
                          target_poll=target_poll, eq=eq,
                          grd=grd2, lamda=0.1, ksi=0) %>% do.call(rbind, .)
      
      # output the predictions for the test data for each fold
      write.csv(pred_test, paste0('data/workingData/gtwr_pred_', csv_name, 
                                  '_fold_', fold_i, '.csv'))
      
   }
   stopCluster(cl)
}