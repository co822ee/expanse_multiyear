gen_df_gtwr <- function(gtwr_model, sp_p, df_p){
   #'@title Generate GTWR predictions
   #'@description generate GTWR predictions for locations with predictor values specified
   #'@param gtwr_model the GTWR model trained by gtwr() in the GWmodel package
   #'@param sp_p the spatial points data frame object for the prediction locations
   #'@param df_p the data frame specifying the predictor values for prediction locations
   #'@param crs_str the crs string for the spatial points
   #'@return data frame object with gtwr as the GTWR predictions
   coef_stack <- gtwr_model$SDF
   gridded(coef_stack) <- T
   coef_stack <- stack(coef_stack)
   # Remove time_stamp 
   coef_stack <- dropLayer(coef_stack, nlayers(coef_stack))
   # extract coefficient values for each point
   coef_df <- lapply(seq_along(sp_p), function(loc_i) raster::extract(coef_stack, sp_p[loc_i,]))
   coef_df <- do.call(rbind, coef_df)
   # Reorder the Intercept to the first column
   coef_df_new <- coef_df[,c(which(colnames(coef_df)=="Intercept"), which(colnames(coef_df)!="Intercept"))]
   predictor_test <- cbind(Intercept=1, df_p %>% dplyr::select(colnames(coef_df_new)[-which(colnames(coef_df_new)=="Intercept")]))  #NOT Assume Intercept is the first one
   gwr_test_pred <- (predictor_test * coef_df_new) %>% apply(., 1, sum)
   cbind(data.frame(gtwr=gwr_test_pred), df_p)
}