calibr_GTWR2 <- function(paramGrid_i, nfold, yrs_v, grd, train_df, param){
   #'@title Perform n-fold CV for tuning the GTWR parameters 
   #'@description generate GTWR predictions for validation points, which are subsetted from the training points
   #'@param fold2_i the fold for n-fold CV for tuning the GTWR parameters
   #'@param nfold the number of (cross-validation) folds for tuning the GTWR parameters
   #'@param grid_i the index for the GTWR parameter grid
   #'@param yrs_v the year vector within the period (eg. for 2008-2010 yrs_v=(2008:2010))
   #'@param grd the spatial regression grid cell of the GTWR
   #'@param train_df the training points
   #'@param param the GTWR parameter grid (with all combinations of GTWR parameters)
   #'@return data frame object of the GTWR predictions at validation points
   #'
   print(paste0('paramGrid_i: ', paramGrid_i))
   # All the validation datasets from the n-fold CV (the CV was divided within training data points)
   gtwr_valid <- lapply(1:nfold, run_GTWR, grid_i=paramGrid_i, nfold, yrs_v, grd=grd,
                        train_df=train_df, param=param) %>% do.call(rbind, .) ## should be the same size as data_all2
   # Remove NA values for gtwr
   gtwr_valid2 <- gtwr_valid[!is.na(gtwr_valid$gtwr), ]
   # Output the predictions at validation points (the validation data that already combines all sub-5-fold validation data)
   cbind(gtwr_valid2, param[paramGrid_i, ])
   #------ 8) Evaluate the n-fold CV performance for GTWR model ------
   # cbind(t(error_matrix(gtwr_valid2$obs, gtwr_valid2$gtwr)), 
   #       param[paramGrid_i, ],
   #       data.frame(ndata=round(nrow(gtwr_valid2)/nrow(gtwr_valid)*100, 1)))  
   ## Need ndata to inspect whether there is enough amount of fold used for CV!
}