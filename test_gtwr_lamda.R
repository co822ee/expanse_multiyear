## This 
library(docstring)
#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(doParallel)
library(foreach)
target_poll = 'NO2'
obs_varname = 'obs'
nfold <- 5
# Multiple single years
csv_names <- paste0('all_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                              '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)
#-----useful function------
source('src/fun_creat_spPoints.R')
source('src/fun_gen_df_gtwr.R')
source('src/fun_trainGTWR.R')
source('src/fun_create_regressionGrid.R')
lamda <- seq(0.2, 1, 0.4)
# five fold CV for GTWR
yr_i <- 7
csv_name <- csv_names[yr_i]
print("********************************************")
print(csv_name)
source("../expanse_multiyear/src/00_fun_read_data_gee.R")

#----Output 2010 surface only (for 2000-2019)
df_sub <- read_data(target_poll, years[[yr_i]])
sp_train <- creat_spPoints(df_sub, 'xcoord', 'ycoord', local_crs)
slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '.csv'))
eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
# Optimize the bandwidth for every year
# Train the GTWR for every year
# Get the predictions for the test data
source('src/fun_outputGTWR.R')
gtwr_model <- outputGTWR(sp_train1=sp_train,
                         target_poll=target_poll, eq=eq,
                         grd=grd2, gtwr_yr = 2010)
source('../EXPANSE_algorithm/scr/fun_plot_gwr_coef.R')
plot_gwr_coef(1, gtwr_model, csv_name=paste0(csv_name, "_defaultParam_", 2010),
              3,3,eu_bnd)

##-------old------
outputGTWRcoef <- function(i){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   
   csv_name <- csv_names[yr_i]
   df_sub <- read_data(target_poll, years[[yr_i]])
   #------ 2) Subset training and test for 5-CV (this should be the same as slr and rf) ------
   # source("src/00_fun_create_fold.R")
   # # The stations will only be included in one specific fold.
   # data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
   #                         multiyear_group = c("sta_code", "year"),
   #                         nfold = 5)
   
   sp_train <- creat_spPoints(df_sub, 'xcoord', 'ycoord', local_crs)
   
   #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '.csv'))
   eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
   
   grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
   
   # Optimize the bandwidth for every year
   # Train the GTWR for every year
   # Get the predictions for the test data
   source('src/fun_outputGTWR.R')
   gtwr_model <- lapply(years[[yr_i]], outputGTWR, sp_train1=sp_train, 
                        target_poll=target_poll, eq=eq,
                        grd=grd2, lamda=lamda[i], ksi=(pi/2))
   gtwr_coef_l <- lapply(gtwr_model, function(gtwr_model){
      gtwr_coef <- gtwr_model$SDF
      gridded(gtwr_coef) <- T
      gtwr_coef <- stack(gtwr_coef)
      gtwr_coef <- dropLayer(gtwr_coef, nlayers(gtwr_coef))
      gtwr_coef
   })
   
   gtwr_coef <- do.call(stack, gtwr_coef_l)
   if(yr_i==7){
      source('../EXPANSE_algorithm/scr/fun_plot_gwr_coef.R')
      lapply(seq_along(gtwr_model), function(j){
         plot_gwr_coef(1, gtwr_model[[j]], csv_name=paste0(csv_name, lamda[i], 'lamda_piD2_', years[[yr_i]][j]),
                       3,3,eu_bnd)
      })
   }
   gtwr_coef
}
gtwr_coef <- lapply(seq_along(lamda), outputGTWRcoef) %>% do.call(stack, .)


param <- expand.grid(lamda=seq(0.1, 1, 0.4),
                     ksi=seq(0, (pi/2), pi/8))

testGTWR2010 <- function(i){
   lamda <- param[i, 'lamda']
   ksi <- param[i, 'ksi']
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   csv_name <- csv_names[yr_i]
   df_sub <- read_data(target_poll, years[[yr_i]])
   #------ 2) Subset training and test for 5-CV (this should be the same as slr and rf) ------
   # source("src/00_fun_create_fold.R")
   # # The stations will only be included in one specific fold.
   # data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"),
   #                         multiyear_group = c("sta_code", "year"),
   #                         nfold = 5)
   sp_train <- creat_spPoints(df_sub, 'xcoord', 'ycoord', local_crs)
   #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '.csv'))
   eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
   grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
   # Optimize the bandwidth for every year
   # Train the GTWR for every year
   # Get the predictions for the test data
   source('src/fun_outputGTWR.R')
   gtwr_model <- outputGTWR(sp_train1=sp_train,
                            target_poll=target_poll, eq=eq,
                            grd=grd2, lamda=lamda, ksi=ksi, gtwr_yr = 2010)
   source('../EXPANSE_algorithm/scr/fun_plot_gwr_coef.R')
   plot_gwr_coef(1, gtwr_model, csv_name=paste0(csv_name, lamda, 'lamda_', ksi,'pi_', 2010),
                 3,3,eu_bnd)
}

lapply((1:nrow(param)), testGTWR2010)


## Compare gtwr with gwr
# Does this setting would result in the same coefficient surface as gwr?
yr_i <- 7
lamda <- 1 
ksi=(pi/2)
csv_name <- csv_names[yr_i]
print("********************************************")
print(csv_name)

source("../expanse_multiyear/src/00_fun_read_data_gee.R")

csv_name <- csv_names[yr_i]
df_sub <- read_data(target_poll, years[[yr_i]])
sp_train <- creat_spPoints(df_sub, 'xcoord', 'ycoord', local_crs)

#------ 3) Read in the predictors selected by SLR (using the same full training data) ------
slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '.csv'))
eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))

grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)

# Optimize the bandwidth for every year
# Train the GTWR for every year
# Get the predictions for the test data
source('src/fun_outputGTWR.R')
gtwr_model <- lapply(years[[yr_i]], outputGTWR, sp_train1=sp_train, 
                     target_poll=target_poll, eq=eq,
                     grd=grd2, lamda=lamda, ksi=ksi)
gtwr_coef_l <- lapply(gtwr_model, function(gtwr_model){
   gtwr_coef <- gtwr_model$SDF
   gridded(gtwr_coef) <- T
   gtwr_coef <- stack(gtwr_coef)
   gtwr_coef <- dropLayer(gtwr_coef, nlayers(gtwr_coef))
   gtwr_coef
})

gtwr_coef <- do.call(stack, gtwr_coef_l)
if(yr_i==7){
   source('../EXPANSE_algorithm/scr/fun_plot_gwr_coef.R')
   lapply(seq_along(gtwr_model), function(j){
      plot_gwr_coef(1, gtwr_model[[j]], csv_name=paste0(csv_name, lamda, 'lamda1_piD2_', years[[yr_i]][j]),
                    3,3,eu_bnd)
   })
}
outputGWR <- function(sp_train1, gtwr_yr, target_poll, eq,
                      grd, lamda=0.1, ksi=0){
   #'@title Train GTWR with specified lamda and ksi
   #'@description Train GTWR with specified lamda and ksi and output the predictions
   #'@param sp_train1 the sp data frame of the training data
   #'@param gtwr_yr the year for training the bandwidth of the GTWR for that year
   #'@param target_poll the pollution of which the concentrations are estimated using GTWR
   #'@param eq the linear regression equation for the GTWR 
   #'@param grd the spatial regression grid cell of the GTWR
   #'@param lamda the scale parameter balancing the effect of spatial and temporal distance 
   #'@param ksi the parameter controlling for the interaction of the space and time effects
   #'@return data frame object with gtwr as the GTWR predictions
   
   print(paste0('gtwr_yr ', gtwr_yr))
   sp_train_sub <- sp_train1[sp_train1$year==gtwr_yr,]
   bw_gtwrYr <- tryCatch(bw.gtwr(eq, sp_train_sub, obs.tv=as.numeric(as.character(sp_train_sub@data$year)),   # st.dMat
                                 lamda = lamda, ksi=ksi,
                                 t.units = 'year',
                                 approach = 'CV', kernel = "exponential", adaptive = T),
                         error=function(e) T)
   if(typeof(bw_gtwrYr)!='logical'){
      #------ 7) Use this optimized bandwidth to train the GTWR for each year with different parameters specified ------
      source("../EXPANSE_algorithm/scr/fun_setupt_gwr.R")
      setup <- setup_gwr(sp_train_sub@data, eu_bnd,
                         cellsize = 200000, local_crs = local_crs, 
                         xcoord="xcoord", ycoord="ycoord")
      DM <- setup[[3]]
      gwr_model <- tryCatch(gwr.basic(eq,
                             data=sp_train_sub, 
                             regression.points=grd2, 
                             adaptive = T,
                             bw=bw_gtwrYr, 
                             dMat=DM,
                             kernel='exponential'),
                            error=function(e)T)
      plot_gwr_coef(fold_i, gwr_model, paste0('gwr_model_', gtwr_yr, '_', target_poll), 
                    n_row = 3, n_col = 3, eu_bnd = eu_bnd)
      return(gtwr_model)
   }
   
}
lapply(years[[yr_i]], outputGWR, sp_train1=sp_train, target_poll=target_poll, 
       eq=eq, grd=grd2, lamda=lamda, ksi=ksi)
