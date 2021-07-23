library(docstring)
#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(doParallel)
library(foreach)
target_poll = 'NO2'
obs_varname = 'obs'
nfold <- 5
# Multiple single years
csv_names <- paste0('o3_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
#-----useful function------
creat_spPoints <- function(data_df, xcoord, ycoord, crs_str){
   #'@title Create sp data frame
   #'@description Create spatial data point from data frame
   #'@param data_df data frame
   #'@param xcoord the colname of the x coordinate in the data_df data frame
   #'@param ycoord the colname of the y coordinate in the data_df data frame
   #'@param crs_str the crs string for the spatial points
   #'@return spatial points data frame object
   #'
   sp::SpatialPointsDataFrame(data = data_df, 
                              coords = cbind(data_df[, xcoord], data_df[, ycoord]),
                              proj4string = crs_str)
}
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
   # Remove time_stamp (so that it would be easier to do modelling in gee)
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
            return(cbind(data.frame(gtwr=NA), valid_sub1))
         }
      }else{
         return(cbind(data.frame(gtwr=NA), valid_sub1))
      }
   }else{
      return(cbind(data.frame(gtwr=NA), valid_sub1))
   }
}
create_regressionGrid <- function(extent_sf, cellsize, crs_str){
   #'@title Output
   #'@description Output 
   #'@param extent_sf the sf object for defining the boundary of the regression grid 
   #'@param cellsize the regression grid cell size  (unit: meter)
   #'@param crs_str the crs string for the spatial points
   #'@return data frame object with gtwr as the GTWR predictions
   xmin <- extent(extent_sf)[1]
   ymin <- extent(extent_sf)[3]
   xmax <- extent(extent_sf)[2]
   ymax <- extent(extent_sf)[4]
   
   
   grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                    c(cellsize,cellsize),
                                    c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)),
                       proj4string = crs_str)
   grd2
}

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