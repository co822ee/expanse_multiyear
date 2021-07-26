## Done: run for NO2 and PM2.5
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

# five fold CV for GTWR
outputGTWRcoef <- function(yr_i){
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

      sp_train <- creat_spPoints(data_all, 'xcoord', 'ycoord', local_crs)
      
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
                          grd=grd2, lamda=0.1, ksi=0)
      gtwr_coef_l <- lapply(gtwr_model, function(gtwr_model){
         gtwr_coef <- gtwr_model$SDF
         gridded(gtwr_coef) <- T
         gtwr_coef <- stack(gtwr_coef)
         gtwr_coef <- dropLayer(gtwr_coef, nlayers(gtwr_coef))
         gtwr_coef
      })
      
      gtwr_coef <- do.call(stack, gtwr_coef_l)
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
