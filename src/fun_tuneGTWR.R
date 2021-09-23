tuneGTWR <- function(sp_train1, gtwr_yr, grid_i, sp_valid1, valid_sub1, target_poll, eq, grd, param){
   #'@title Train GTWR for a specific year using observations from multiple years
   #'@description train GTWR for a specific year using observations from multiple years
   #'@param sp_train1 the spatial points data frame object for the training data
   #'@param grid_i the index for the GTWR parameter grid
   #'@param sp_valid1 the spatial points data frame object for the validation data
   #'@param valid_sub1 the data frame for the validation data
   #'@param target_poll the air pollutant
   #'@param eq the equation
   #'@param grd the spatial regression grid cell of the GTWR
   #'@param param the GTWR parameter grid (with all combinations of GTWR parameters)
   #'@return data frame object with gtwr as the GTWR predictions
  
   sp_valid_sub <- sp_valid1[sp_valid1$year==gtwr_yr,]
   valid_sub1 <- valid_sub1[valid_sub1$year==gtwr_yr,]
   
   source('src/fun_outputGTWR.R')
   gtwr_model <- outputGTWR(sp_train1, gtwr_yr, target_poll, eq, grd, 
                            param$lamda[grid_i], 
                            param$ksi[grid_i],
                            param$conv_dist[grid_i]
   )
   
   if(typeof(gtwr_model)!='logical'&(!is.null(gtwr_model))){
      if(!any(is.na(gtwr_model$SDF[1]$Intercept))){
         gtwr_valid <- gen_df_gtwr(gtwr_model, sp_valid_sub, valid_sub1)
         return(gtwr_valid)
      }else{
         return(cbind(data.frame(gtwr=NA), valid_sub1))
      }
   }else{
      return(cbind(data.frame(gtwr=NA), valid_sub1))
   }
   
   
}