# Try gtwr
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
tuneRF_b = F
target_poll = 'NO2'
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
source("src/00_fun_create_fold.R")
# The stations will only be included in one specific fold.
data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                        multiyear_group = c("sta_code", "year"),
                        nfold = 5)
test_sub <- data_all[data_all$nfold==fold_i,]
train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.


# train_data <- train_sub[train_sub$n_obs==length(years[[yr_i]]),]

sp_train <- sp::SpatialPointsDataFrame(data = train_sub,
                                       coords = cbind(train_sub[, "xcoord"], train_sub[, "ycoord"]),
                                       proj4string = local_crs)
sp_test <- sp::SpatialPointsDataFrame(data = test_sub,
                                       coords = cbind(test_sub[, "xcoord"], test_sub[, "ycoord"]),
                                       proj4string = local_crs)
#read in the predictors selected by SLR
slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '_fold_', fold_i, '.csv'))
eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
#calibrate bandwidth (bandwidth optimization): spatial and temporal bandwidth
# (obtain the spatio-temporal decaying bandwidth from the data points) 
# Define the spatio-temporal distance matrix (st.dMat) for regression points and data points
# 1) define the spatial distance matrix
xmin <- extent(eu_bnd)[1]
ymin <- extent(eu_bnd)[3]
xmax <- extent(eu_bnd)[2]
ymax <- extent(eu_bnd)[4]
cellsize <- 200000

grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                 c(cellsize,cellsize),
                                 c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)),
                    proj4string = local_crs)

# stdMat = gw.dist(dp.locat=coordinates(sp_train),
#                  rp.locat=coordinates(grd2))
get_stdMatrix <- function(dp, rp, rp_yr){
   dp1 <- cbind(coordinates(dp), as.numeric(as.character(dp@data$year)))#[1:5,]
   rp1 <- cbind(coordinates(rp), rp_yr)#[1:2,]
   dprp <- expand_grid(dp1, rp1)
   dist_diff <- dprp[,1]-dprp[,2]
   dist_v <- sqrt(apply(dist_diff*dist_diff, 1, sum))
   stdMat <- matrix(dist_v, nrow=length(dp), byrow=T)
   stdMat
}

# stdMat <- get_stdMatrix(sp_train, grd2, gtwr_yr)

# 2) spatio-temporal bandwidth was calibrated
# (does it give a different result if we look at the stations are not available back in time? Yes)
# bw=19 if only use ones that are available across the whole time period
# bw=21 if include all data
# We should use all data
# No regression points involved
# stMat_dp <- get_stdMatrix(sp_train, sp_train, gtwr_yr)
bw <- bw.gtwr(eq, sp_train, obs.tv=as.numeric(as.character(sp_train@data$year)),   # st.dMat
              st.dMat = st.dist(dp.locat = coordinates(sp_train),
                                obs.tv = as.numeric(as.character(sp_train@data$year)),
                                lamda=0.9, ksi=0.8),
              lamda = 0.9,
              ksi=0.8,
              approach = 'CV', kernel = "exponential", adaptive = T)
bw
## [1] 16
# Should we use all data points to calibrate the bandwidth?
# -> No, we should use data points from each year to calibrate the bandwidth
#    and then use the corresponding bandwidth to estimate the GTWR regression surface for that year
#    The rationale of using different bandwidth for different years is that 
#    in early years when the number of observations is few, the bandwidth should be
#    larger so that the model would become more 'global' to decrease the bias

sp_train_sub <- sp_train[sp_train$year==gtwr_yr,]
bw_gtwrYr <- bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        lamda = 0.9,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 39  (yr_i=2); 25 (yr_i=7)

sp_train_sub <- sp_train[sp_train$year%in%seq(gtwr_yr-1, gtwr_yr+1, 1),]
bw_gtwrYr <- bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
                     lamda = 0.9,
                     approach = 'CV', kernel = "exponential", adaptive = T)


sp_train_sub <- sp_train[sp_train$year==2009,]
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        lamda = 0.9,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 39 (yr_i=2); 24  (yr_i=7)
# sp_train_sub <- sp_train[sp_train$year==2000,]
# bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
#         approach = 'CV', kernel = "exponential", adaptive = T)
# # [1] 32 (yr_i=7)
# bw.gwr(eq, sp_train, approach = 'CV', kernel = 'exponential', adaptive = T,
#        dMat = gw.dist(dp.locat=coordinates(sp_train),
#                       rp.locat=coordinates(sp_train)))

# The problem here is how we should set the spatio-temporal scale factors
# to balance the different effects that measure the spatial and temporal dsitance.
# The parameter ksi controls the interaction of space and time effects. 
# The default value is zero, meaning the time and space have the maximal effects.
# lamda=0.05
# ksi=0
#--> optimize parameter
sp_train_sub <- sp_train[sp_train$year==2010,]
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 24  (yr_i=7)
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        ksi=0,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 24  (yr_i=7)
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        ksi=0, lamda=0.01,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 24  (yr_i=7)
## A time period to consider the time influence as well
sp_train_sub <- sp_train[sp_train$year%in%(2007:2009),]
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        lamda = 0.9,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 20  (yr_i=7)
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        ksi=0,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 20  (yr_i=7)
bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
        ksi=0, lamda=0.01,
        approach = 'CV', kernel = "exponential", adaptive = T)
## [1] 20  (yr_i=7)


# spatio-temporal weight matrix (spatio-temporal distance matrix: st.dMat)
## Weights for data points are derived from not only distance between the regression point
## and each data point, but also based on the separation in time between them.
## gw.dist for gwr
## is needed for gtwr as st.dMat (pre-specified spatio-temporal distance matrix)
## With regression points involved
## Use grd
gtwr_model <- gtwr(eq, data=sp_train, st.bw=bw_gtwrYr, regression.points = grd2, 
                   # st.dMat=stdMat,
                   st.dMat=st.dist(dp.locat=coordinates(sp_train), rp.locat=coordinates(grd2), 
                                   obs.tv=as.numeric(as.character(sp_train@data$year)), 
                                   reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),
                                   lamda=1, t.units = 'year'),
                   lamda = 1, t.units = 'year',
                   obs.tv=as.numeric(as.character(sp_train@data$year)),   ## time stamps for the observation points
                   reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),     ## time stamps for the regression points
                   adaptive=T, kernel='exponential')


# gtwr_model$SDF
gtwr_model

gtwr_coef <- gtwr_model$SDF
gridded(gtwr_coef) <- T
gtwr_coef <- stack(gtwr_coef)
# Remove time_stamp (so that it would be easier to do modelling in gee)
gtwr_coef <- dropLayer(gtwr_coef, nlayers(gtwr_coef))
writeRaster(gtwr_coef, paste0('data/temp/test_gwr_coef_', csv_name, '_', gtwr_yr, '.tiff'),
            overwrite=T)

gtwr_model <- gtwr(eq, data=sp_train, st.bw=bw, regression.points = grd2, 
                   # st.dMat=stdMat,
                   st.dMat=st.dist(dp.locat=coordinates(sp_train), rp.locat=coordinates(grd2),
                                   obs.tv=as.numeric(as.character(sp_train@data$year)),
                                   reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),
                                   lamda=0.9, ksi=0.8),
                   lamda = 0.9, ksi=0.8,
                   obs.tv=as.numeric(as.character(sp_train@data$year)),   ## time stamps for the observation points
                   reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),     ## time stamps for the regression points
                   adaptive=T, kernel='exponential')
gtwr_model2 <- gtwr(eq, data=sp_train, st.bw=bw, regression.points = grd2, 
                   # st.dMat=stdMat,
                   st.dMat=st.dist(dp.locat=coordinates(sp_train), rp.locat=coordinates(grd2),
                                   obs.tv=as.numeric(as.character(sp_train@data$year)),
                                   reg.tv=rep(2001, nrow(coordinates(grd2))),
                                   lamda=0.9, ksi=0.8),
                   lamda = 0.9, ksi=0.8,
                   obs.tv=as.numeric(as.character(sp_train@data$year)),   ## time stamps for the observation points
                   reg.tv=rep(2001, nrow(coordinates(grd2))),     ## time stamps for the regression points
                   adaptive=T, kernel='exponential')
gtwr_model
gtwr_model2
gtwr_model$SDF
gtwr_model2$SDF

source('../EXPANSE_algorithm/scr/fun_plot_gwr_coef.R')
plot_gwr_coef(fold_i, gtwr_model, 'gtwr_model2019',3,3,eu_bnd)
plot_gwr_coef(fold_i, gtwr_model2, 'gtwr_model2000',3,3,eu_bnd)

for(gtwr_yr in years[[yr_i]]){
   gtwr_model_yr <- gtwr(eq, data=sp_train, st.bw=bw, regression.points = grd2, 
                         # st.dMat=stdMat,
                         st.dMat=st.dist(dp.locat=coordinates(sp_train), rp.locat=coordinates(grd2),
                                         obs.tv=as.numeric(as.character(sp_train@data$year)),
                                         reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),
                                         lamda=0.9, ksi=0.8),
                         lamda = 0.9, ksi=0.8,
                         obs.tv=as.numeric(as.character(sp_train@data$year)),   ## time stamps for the observation points
                         reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),     ## time stamps for the regression points
                         adaptive=T, kernel='exponential')
   plot_gwr_coef(fold_i, gtwr_model_yr, 
                 paste0('gtwr_model_', gtwr_yr),3,3,eu_bnd)
   # output stack of rasters (remove timestep)
   
}
source("../EXPANSE_algorithm/scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd,
                   cellsize = 200000, local_crs = local_crs, 
                   xcoord="xcoord", ycoord="ycoord")
DM <- setup[[3]]
source("../EXPANSE_algorithm/scr/fun_gwr.R")
gwr_model <- gwr(sp_train, grd2, DM, bw, paste0(csv_name, '_fold_', fold_i))

plot_gwr_coef(fold_i, gwr_model, paste0(csv_name, '_fold_', fold_i), 
              n_row = 3, n_col = 3, eu_bnd = eu_bnd)

# Check the prediction values (validation)
sp_test[i,]@data$year %>% as.numeric
r_c <- gtwr_model$SDF
gridded(r_c) <- T
r_c <- stack(r_c)
i=12
((extract(r_c, sp_train[i,])*cbind(1, sp_train[i,]@data %>% dplyr::select(names(r_c)[-c(1, length(names(r_c)))]),
                                   0))[1,] %>% sum)
((extract(r_c, sp_train[i,])*cbind(1, sp_train[i,]@data %>% dplyr::select(names(r_c)[-c(1, length(names(r_c)))]),
                                   0))[1,] %>% sum)-sp_train[i,]@data$obs
((extract(r_c, sp_test[i,])*cbind(1, sp_test[i,]@data %>% dplyr::select(names(r_c)[-c(1, length(names(r_c)))]),
                                   0))[1,] %>% sum)
((extract(r_c, sp_test[i,])*cbind(1, sp_test[i,]@data %>% dplyr::select(names(r_c)[-c(1, length(names(r_c)))]),
                                   0))[1,] %>% sum)-sp_test[i,]@data$obs


## If ksi is set as 0, then basically the parameter surfaces from two different years 
## are the same.
#• Check functions within gtwr
# st.disti <- st.dist(dp.locat=coordinates(sp_train), rp.locat=coordinates(grd2), 
#                     obs.tv=as.numeric(as.character(sp_train@data$year)), 
#                     reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),
#                     lamda=1)
# W.i <- gw.weight(st.disti, bw, kernel='exponential', adaptive=T)
# W.i[1,1]
# gw.weight(st.disti, 42, kernel='exponential', adaptive=T)[1,1]
# dim(st.disti)
# dim(W.i)
#• Inspect the stdMatrix
stdMat_f <- st.dist(dp.locat=coordinates(sp_train), rp.locat=coordinates(grd2), 
                    obs.tv=as.numeric(as.character(sp_train@data$year)), 
                    reg.tv=rep(gtwr_yr, nrow(coordinates(grd2))),
                    lamda=1)

gw.dist(dp.locat=coordinates(sp_train),
        rp.locat=coordinates(grd2))[1]
stdMat_f[1, 1]
stdMat <- get_stdMatrix(sp_train, grd2, gtwr_yr)
stdMat[1, 1]


