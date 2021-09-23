outputGTWR <- function(sp_train1, gtwr_yr, target_poll, eq,
                      grd, lamda=0.1, ksi=0, conv_dist=1000){
   #'@title Train GTWR with specified lamda and ksi
   #'@description Train GTWR with specified lamda and ksi and output the predictions
   #'@param sp_train1 the sp data frame of the training data
   #'@param gtwr_yr the year for training the bandwidth of the GTWR for that year
   #'@param target_poll the pollution of which the concentrations are estimated using GTWR
   #'@param eq the linear regression equation for the GTWR 
   #'@param grd the spatial regression grid cell of the GTWR
   #'@param lamda the scale parameter balancing the effect of spatial and temporal distance 
   #'@param ksi the parameter controlling for the interaction of the space and time effects
   #'@param ksi the parameter of the equivalent spatial distance for the 1-year temporal distance 
   #'@return data frame object with gtwr as the GTWR predictions
   
   print(paste0('gtwr_yr ', gtwr_yr))
   if(target_poll=='PM2.5'|target_poll=='PM10'){
      sp_train_sub <- sp_train1[sp_train1$year%in%seq(gtwr_yr-1, gtwr_yr+1, 1),]
   }else{
      sp_train_sub <- sp_train1[sp_train1$year==gtwr_yr,]
   }
   #------ 6) Optimize the bandwidth for each year using the training data with different parameters specified ------
   #calibrate bandwidth (bandwidth optimization): spatial and temporal bandwidth
   # (obtain the spatio-temporal decaying bandwidth from the data points) 
   # to define the spatio-temporal distance matrix (st.dMat) for regression points and data points
   
   bw_gtwrYr <- tryCatch(bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
                                 lamda = lamda, ksi=ksi,
                                 t.units = 'years',
                                 st.dMat=st.dist(dp.locat=coordinates(sp_train_sub), rp.locat=coordinates(sp_train_sub), 
                                                 obs.tv=as.numeric(as.character(sp_train_sub@data$year)), 
                                                 reg.tv=as.numeric(as.character(sp_train_sub@data$year)),
                                                 lamda = lamda, ksi=ksi,
                                                 t.dMat = abs(outer(sort(unique(as.numeric(as.character(sp_train_sub@data$year)))),
                                                                    sort(unique(as.numeric(as.character(sp_train_sub@data$year)))), '-'))*conv_dist,    # One year apart between observations from the same stations in time 
                                                                                                                                                   #  is equal to one kilometer apart from the same year in space
                                                 t.units = 'years'),
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
                                                  t.dMat = abs(outer(sort(unique(as.numeric(as.character(sp_train1@data$year)))),
                                                                     sort(unique(rep(2010, nrow(coordinates(grd))))), '-'))*conv_dist,  # One year apart between observations from the same stations in time 
                                                                                                                                  #  is equal to one kilometer apart from the same year in space
                                                  t.units = 'years'),
                                  lamda = lamda, ksi=ksi,
                                  t.units = 'years',
                                  obs.tv=as.numeric(as.character(sp_train1@data$year)),   ## time stamps for the observation points
                                  reg.tv=rep(gtwr_yr, nrow(coordinates(grd))),     ## time stamps for the regression points
                                  adaptive=T, kernel='exponential'), 
                             error=function(e) T)
      return(gtwr_model)
   }else{
      return(NULL)
   }
   
}



