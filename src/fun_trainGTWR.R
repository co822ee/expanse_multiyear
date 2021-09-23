trainGTWR <- function(sp_train1, gtwr_yr, sp_valid1, valid_sub1, target_poll, eq,
                      grd, lamda=0.1, ksi=0){
   #'@title Train GTWR with specified lamda and ksi
   #'@description Train GTWR with specified lamda and ksi and output the predictions
   #'@param sp_train1 the sp data frame of the training data
   #'@param gtwr_yr the year for training the bandwidth of the GTWR for that year
   #'@param sp_valid1 the sp data frame of the validation data which would be used for prediction
   #'@param valid_sub1 the data frame of the validation data
   #'@param target_poll the pollution of which the concentrations are estimated using GTWR
   #'@param eq the linear regression equation for the GTWR 
   #'@param grd the spatial regression grid cell of the GTWR
   #'@param lamda the scale parameter balancing the effect of spatial and temporal distance 
   #'@param ksi the parameter controlling for the interaction of the space and time effects
   #'@return data frame object with gtwr as the GTWR predictions
   
   print(paste0('gtwr_yr ', gtwr_yr))
   if(target_poll=='PM2.5'|target_poll=='PM10'){
      sp_train_sub <- sp_train1[sp_train1$year%in%seq(gtwr_yr-1, gtwr_yr+1, 1),]
   }else{
      sp_train_sub <- sp_train1[sp_train1$year==gtwr_yr,]
   }
   sp_valid_sub <- sp_valid1[sp_valid1$year==gtwr_yr,]
   valid_sub1 <- valid_sub1[valid_sub1$year==gtwr_yr,]
   #------ 6) Optimize the bandwidth for each year using the training data with different parameters specified ------
   #calibrate bandwidth (bandwidth optimization): spatial and temporal bandwidth
   # (obtain the spatio-temporal decaying bandwidth from the data points) 
   # to define the spatio-temporal distance matrix (st.dMat) for regression points and data points
   
   bw_gtwrYr <- tryCatch(bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
                                 lamda = lamda, ksi=ksi,
                                 t.units = 'year',
                                 approach = 'CV', kernel = "exponential", adaptive = T),
                         error=function(e) T)
   if(typeof(bw_gtwrYr)!='logical'){
      #------ 7) Use this optimized bandwidth to train the GTWR for each year with different parameters specified ------
      gtwr_model <- tryCatch(gtwr(eq, data=sp_train1, st.bw=bw_gtwrYr, regression.points = grd, 
                                  # st.dMat=stdMat,
                                  st.dMat=st.dist(dp.locat=coordinates(sp_train1), rp.locat=coordinates(grd), 
                                                  obs.tv=as.numeric(as.character(sp_train1@data$year)), 
                                                  reg.tv=rep(gtwr_yr, nrow(coordinates(grd))),
                                                  lamda = lamda, ksi=ksi,
                                                  t.units = 'year'),
                                  lamda = lamda, ksi=ksi,
                                  t.units = 'year',
                                  obs.tv=as.numeric(as.character(sp_train1@data$year)),   ## time stamps for the observation points
                                  reg.tv=rep(gtwr_yr, nrow(coordinates(grd))),     ## time stamps for the regression points
                                  adaptive=T, kernel='exponential'), 
                             error=function(e) T)
      if(typeof(gtwr_model)!='logical'){
         if(!any(is.na(gtwr_model$SDF[1]$Intercept))){
            gtwr_valid <- gen_df_gtwr(gtwr_model, sp_valid_sub, valid_sub1)
            return(gtwr_valid)
         }else{
            if(nrow(valid_sub1)==0){
               
               return(cbind(data.frame(gtwr=NA), valid_sub1[1,]))
            }else{
               return(cbind(data.frame(gtwr=NA), valid_sub1))
            }
         }
      }else{
         return(cbind(data.frame(gtwr=NA), valid_sub1))
      }
   }else{
      return(cbind(data.frame(gtwr=NA), valid_sub1))
   }
}