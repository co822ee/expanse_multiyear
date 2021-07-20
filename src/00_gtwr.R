#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
tuneRF_b = F
target_poll = 'PM2.5'
obs_varname = 'obs'
# Multiple single years
csv_names <- paste0('o3_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
nfold <- 5
yr_i <- 7
gtwr_yr <- 2010
csv_name <- csv_names[yr_i]
print("********************************************")
print(csv_name)

source("../expanse_multiyear/src/00_fun_read_data_gee.R")

csv_name <- csv_names[yr_i]
df_sub <- read_data(target_poll, years[[yr_i]])
fold_i <- 1
## ------ *** useful function -----
creat_spPoints <- function(data_df, xcoord, ycoord, crs_str){
   sp::SpatialPointsDataFrame(data = data_df, 
                              coords = cbind(data_df[, xcoord], data_df[, ycoord]),
                              proj4string = crs_str)
}
create_regressionGrid <- function(extent_sf, cellsize, crs_str){
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
gen_df_gtwr <- function(gtwr_model, sp_p, df_p, forstepwise=F){
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
   if(!forstepwise){
      gwr_test_df <- cbind(data.frame(gtwr=gwr_test_pred), df_p)
   }else{
      gwr_test_df <- gwr_test_pred
   }
   gwr_test_df
}
## Train GTWR for each year
trainGTWR <- function(sp_train1, gtwr_yr, grid_i, sp_valid1, valid_sub1, target_poll){
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
                                 lamda = gtwr_param$lamda[grid_i],
                                 ksi = gtwr_param$ksi[grid_i],
                                 t.units = 'year',
                                 approach = 'CV', kernel = "exponential", adaptive = T),
                         error=function(e) T)
   if(typeof(bw_gtwrYr)!='logical'){
      #------ 7) Use this optimized bandwidth to train the GTWR for each year with different parameters specified ------
      gtwr_model <- gtwr(eq, data=sp_train1, st.bw=bw_gtwrYr, regression.points = grd2, 
                         # st.dMat=stdMat,
                         st.dMat=st.dist(dp.locat=coordinates(sp_train1), rp.locat=coordinates(grd2), 
                                         obs.tv=as.numeric(as.character(sp_train1@data$year)), 
                                         reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),
                                         lamda = gtwr_param$lamda[grid_i],
                                         ksi = gtwr_param$ksi[grid_i], 
                                         t.units = 'year'),
                         lamda = gtwr_param$lamda[grid_i],
                         ksi = gtwr_param$ksi[grid_i], 
                         t.units = 'year',
                         obs.tv=as.numeric(as.character(sp_train1@data$year)),   ## time stamps for the observation points
                         reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),     ## time stamps for the regression points
                         adaptive=T, kernel='exponential')
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
#------ 2) Subset training and test for 5-CV (this should be the same as slr and rf) ------
source("src/00_fun_create_fold.R")
# The stations will only be included in one specific fold.
data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                        multiyear_group = c("sta_code", "year"),
                        nfold = 5)
test_sub <- data_all[data_all$nfold==fold_i,]
train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.

sp_train <- creat_spPoints(train_sub, 'xcoord', 'ycoord', local_crs)
sp_test <- creat_spPoints(test_sub, 'xcoord', 'ycoord', local_crs)

#------ 3) Read in the predictors selected by SLR (using the same full training data) ------


slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '_fold_', fold_i, '.csv'))
eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))

grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
# Create a grid of parameters (lamda & ksi)  using expand.grid
gtwr_param <- expand.grid(lamda=seq(0, 1, 0.05), ksi=seq(0, pi/2, pi/16))

#------ 4) Subset training data into training and validation data ------
paramGrid_i <- 4  # Loop
fold2_i <- 1  # Loop
run_GTWR <- function(fold2_i, grid_i, yrs_v){
   print('------------------------------')
   print(paste0('fold ', fold2_i))
   train_sub <- train_sub %>% dplyr::select(-c('nfold', 'n_obs', 'index'))
   data_all2 <- create_fold(train_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                            multiyear_group = c("sta_code", "year"),
                            nfold = 5)
   valid_sub <- data_all2[data_all2$nfold==fold2_i,]
   train_sub <- data_all2[-valid_sub$index, ] #data_all$index starts from 1 to the length.
   
   sp_train <- creat_spPoints(train_sub, 'xcoord', 'ycoord', local_crs)
   sp_valid <- creat_spPoints(valid_sub, 'xcoord', 'ycoord', local_crs)
   
   # Run the GTWR for all years in the period
   gtwr_valid <- lapply(yrs_v, trainGTWR, sp_train1=sp_train, grid_i=grid_i,
                        sp_valid1=sp_valid, valid_sub1=valid_sub, target_poll=target_poll) %>% do.call(rbind, .)  ## Should be the same size as validation data
   gtwr_valid
}


# end the fold2_i loop
#------ 8) Evaluate the performance for GTWR model ------
# Remove NA values for gtwr
calibr_GTWR <- function(paramGrid_i, nfold, yrs_v){
   gtwr_valid <- lapply(1:nfold, run_GTWR, grid_i=paramGrid_i, yrs_v) %>% do.call(rbind, .) ## should be the same size as data_all2
   gtwr_valid2 <- gtwr_valid[!is.na(gtwr_valid$gtwr), ]
   cbind(t(error_matrix(gtwr_valid2$obs, gtwr_valid2$gtwr)), gtwr_param[paramGrid_i, ])
}
final_perfm <- lapply(1:(nrow(gtwr_param)), calibr_GTWR, nfold=nfold, yrs_v=years[[yr_i]])


#------ 9) Find the parameter set which gives the best RMSE/R2 ------

# Save this parameter optimization results