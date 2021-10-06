# This script runs the 5-fold CV for GTWR

#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
target_poll = 'NO2'
obs_varname = 'obs'
# Multiple single years
csv_names <- paste0('o3_',target_poll, "_",c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19', '00-19'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2012:2019, 2000:2019)

outputGTWRcoef <- function(yr_i){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   #---------- read in data ------
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   df_sub <- read_data(target_poll, years[[yr_i]])
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   pred_c <- select_predictor(df_sub)
   #--------- optimized parameters ------
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', csv_name))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   opt_lamda <- gtwr_param$lamda[which.max(gtwr_param$rsq)]
   opt_ksi <- gtwr_param$ksi[which.max(gtwr_param$rsq)]
   opt_convDist <- gtwr_param$conv_dist[which.max(gtwr_param$rsq)]
   
   sp_train <- creat_spPoints(df_sub, 'xcoord', 'ycoord', local_crs)
   #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '.csv'))
   eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
   
   grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
   
   #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, '.csv'))
   eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
   
   grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
   
   source('src/fun_outputGTWR.R')
   
   gtwr_model <- lapply(years[[yr_i]], outputGTWR, sp_train1=sp_train, 
                        target_poll=target_poll, eq=eq,
                        grd=grd2, lamda=opt_lamda, ksi=opt_ksi, conv_dist=opt_convDist)
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
      lapply(seq_along(gtwr_model), function(i){
         plot_gwr_coef(1, gtwr_model[[i]], csv_name=paste0(csv_name, '_', years[[yr_i]][i]),
                       3,3,eu_bnd)
      })
   }
   gtwr_coef
}
gtwr_coef <- lapply(seq_along(csv_names), outputGTWRcoef) %>% do.call(stack, .)
# output the parameter surfaces
if(!dir.exists('data/workingData/gtwr_coef/')) dir.create('data/workingData/gtwr_coef/')
writeRaster(gtwr_coef, paste0('data/workingData/gtwr_coef/gtwr_', target_poll,'.tiff'),
            overwrite=T)
# Check the amount of layers with slr multiple year results
read.csv('data/processed/combined/SLRcoef_all_multiYears.csv') %>% filter(poll=='NO2') %>% dim()
dim(gtwr_coef)
