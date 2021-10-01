run_GTWR <- function(fold2_i, nfold, grid_i, yrs_v, grd, train_df, param){
   #'@title Run GTWR for every year in the time period for tuning the GTWR parameters
   #'@description generate GTWR predictions for validation points, which are subsetted from the training points
   #'@param fold2_i the fold for n-fold CV for tuning the GTWR parameters
   #'@param nfold the number of (cross-validation) folds for tuning the GTWR parameters
   #'@param grid_i the index for the GTWR parameter grid
   #'@param yrs_v the year vector within the period (eg. for 2008-2010 yrs_v=(2008:2010))
   #'@param grd the spatial regression grid cell of the GTWR
   #'@param train_df the training points
   #'@param param the GTWR parameter grid (with all combinations of GTWR parameters: ksi, lamda & conv_dist)
   #'@return data frame object of the GTWR predictions at validation points
   #'
   #------ 4) Subset training data into training and validation data ------
   print('------------------------------')
   print(paste0('fold ', fold2_i))
   train_sub2 <- train_df %>% dplyr::select(-c('nfold', 'n_obs', 'index'))
   data_all2 <- create_fold(train_sub2, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                            multiyear_group = c("sta_code", "year"),
                            nfold = nfold)
   valid_sub <- data_all2[data_all2$nfold==fold2_i,]
   train_sub2 <- data_all2[-valid_sub$index, ] #data_all$index starts from 1 to the length.
   
   sp_train2 <- creat_spPoints(train_sub2, 'xcoord', 'ycoord', local_crs)
   sp_valid <- creat_spPoints(valid_sub, 'xcoord', 'ycoord', local_crs)
   
   source('src/fun_tuneGTWR.R')
   # Run the GTWR for all years in the period
   gtwr_valid <- lapply(yrs_v, tuneGTWR, sp_train1=sp_train2, grid_i=grid_i,
                        sp_valid1=sp_valid, valid_sub1=valid_sub, 
                        target_poll=target_poll, eq=eq, grd=grd, 
                        param=param) %>% do.call(rbind, .)  ## Should be the same size as validation data
   gtwr_valid
}