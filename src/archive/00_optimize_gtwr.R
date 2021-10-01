# This script tunes the parameters for GTWR (lamda, ksi, and the equivalent temporal distance)
# Stop at 06-12
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


########### ------ optimize the parameters ------------ #######
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
      source('src/fun_creat_spPoints.R')
      source('src/fun_create_regressionGrid.R')
      source('src/fun_gen_df_gtwr.R')
      source('src/fun_run_GTWR.R')
      source('src/fun_calibr_GTWR.R')
      
      #------------------------------------------------------------
      test_sub <- data_all[data_all$nfold==fold_i,]
      train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
      
      sp_train <- creat_spPoints(train_sub, 'xcoord', 'ycoord', local_crs)
      sp_test <- creat_spPoints(test_sub, 'xcoord', 'ycoord', local_crs)
      
      #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
      
      
      slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '_fold_', fold_i, '.csv'))
      eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
      
      grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
      # Create a grid of parameters (lamda & ksi)  using expand.grid
      gtwr_param <- expand.grid(lamda=seq(0.2, 1, 0.4), 
                                ksi=seq(0, pi/2, pi/4), 
                                conv_dist=c(0.3, 0.6, 1, 5, seq(10, 50, 10))*1000)  # unit of conv_dist: meter/time-unit (here the time-unit=year)
      
      # Output the 5-CV performance of each combination of the GTWR parameters (ksi, lamda, conv_dist)
      final_perfm <- lapply(1:(nrow(gtwr_param)), calibr_GTWR, nfold=nfold, 
                            yrs_v=years[[yr_i]], grd=grd2, train_df=train_sub, param=gtwr_param)
      final_perfm2 <- final_perfm %>% do.call(rbind, .)
      
      write.csv(final_perfm2, paste0('data/workingData//gtwr_param_', csv_name, '_fold_', fold_i, '.csv'), row.names = F)
      
      #------ 9) Find the parameter set which gives the best RMSE/R2 ------
      # lamda_final <- (final_perfm2 %>% arrange(desc(ndata), desc(rsq)))[1,]$lamda
      # ksi_final <- (final_perfm2 %>% arrange(desc(ndata), desc(rsq)))[1,]$ksi
   }
   stopCluster(cl)
}
## Does it necessary to calibrate the parameters?
## [Result 1: only calibrating ksi and lamda without setting conv_dist (default conv_dist=1)]
## It seems that ksi=0 and lamda=0.1 would give the best result
## although the difference between different parameter values is rather small.

# inspect the results


# Save this parameter optimization results