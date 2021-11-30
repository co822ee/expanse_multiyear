# This script runs the 5-fold CV for GTWR to output the coefficient surfaces for only 2000-2019

#------ 1) Read in data ------
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(doParallel)
library(foreach)
target_polls = c('NO2', 'O3', 'PM10', 'PM2.5')
obs_varname = 'obs'
# Multiple single years
csv_names <- paste0('o3_',target_polls, "_",c('00-19'))   #2008:2012
years <- list(2000:2019)
nfold <- 5
# gtwr_yr <- 2010
# fold_i <- 1
## ------ *** useful function -----


for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   target_poll <- target_polls[yr_i]
   print("********************************************")
   print(csv_name)
   #---------- read in data ------
   source("../expanse_multiyear/src/00_fun_read_data_gee.R")
   df_sub <- read_data(target_poll, years[[1]])
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   pred_c <- select_predictor(df_sub)
   #--------- optimized parameters ------
   gtwr_files <- list.files('data/workingData/', paste0('gtwr_param_', csv_name))
   param_l <- lapply(paste0('data/workingData/', gtwr_files), read.csv)
   gtwr_param <- do.call(rbind, param_l)
   
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
      source('src/fun_creat_spPoints.R')
      source('src/fun_create_regressionGrid.R')
      csv_name_fold <- paste0(csv_name, '_fold_', fold_i)
      if(!file.exists(paste0('data/workingData/gtwr_coef/', csv_name_fold, '.tif'))){
         test_sub <- data_all[data_all$nfold==fold_i,]
         train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
         
         sp_train <- creat_spPoints(train_sub, 'xcoord', 'ycoord', local_crs)
         sp_test <- creat_spPoints(test_sub, 'xcoord', 'ycoord', local_crs)
         
         #------ 3) Read in the predictors selected by SLR (using the same full training data) ------
         slr <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name_fold, '.csv'))
         eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
         
         grd2 <- create_regressionGrid(eu_bnd, 200000, local_crs)
         
         # Output the predictions at validation points using GTWR 
         source('src/fun_outputGTWR.R')
         gtwr_model <- lapply(years[[1]], function(gtwr_yr){
            grid_i <- which.max(gtwr_param$rsq)
            gtwr_model_yr <- outputGTWR(sp_train, gtwr_yr, 
                                        target_poll, eq, grd2, 
                                        gtwr_param$lamda[grid_i], 
                                        gtwr_param$ksi[grid_i],
                                        gtwr_param$conv_dist[grid_i]
            )
            gtwr_model_yr
         })
         ## nfold=4 PM2.5 from year 2000-2003 (00-19) there are NAs
         ## (because the error of optimizing the bandwidth size: error: inv(): matrix seems singular)
         if((target_poll=='PM2.5')&(fold_i==4)){
            slr_all <- data.frame(variables=slr$variables, 
                                  year=rep(years[[1]][-which(unlist(lapply(gtwr_model, is.null)))], each=nrow(slr)),
                                  poll=target_poll, fold=fold_i, csv_name=csv_name_fold)
         }else{
            slr_all <- data.frame(variables=slr$variables, year=rep(years[[1]], each=nrow(slr)),
                                  poll=target_poll, fold=fold_i, csv_name=csv_name_fold)
         }
         if((target_poll=='PM2.5')&(fold_i==4)){
            gtwr_model <- gtwr_model[-which(unlist(lapply(gtwr_model, is.null)))]
         }
         gtwr_coef_l <- lapply(gtwr_model, function(gtwr_model){
            gtwr_coef <- gtwr_model$SDF
            gridded(gtwr_coef) <- T
            gtwr_coef <- stack(gtwr_coef)
            gtwr_coef <- dropLayer(gtwr_coef, nlayers(gtwr_coef))
            gtwr_coef
         })
         
         slr_all$index = 1:nrow(slr_all)
         gtwr_coef <- do.call(stack, gtwr_coef_l)
         write.csv(slr_all, paste0('data/workingData/gtwr_coef/slr_', csv_name_fold, '.csv'),
                   row.names=F)
         writeRaster(gtwr_coef, paste0('data/workingData/gtwr_coef/', csv_name_fold, '.tif'),
                     overwrite=T)
      }
   }
   stopCluster(cl)
   gc()
}

all_coefslr <- lapply(csv_names, function(csv_name){
   filenames <- list.files('data/workingData/gtwr_coef/', csv_name)
   filenames_slr <- filenames[grepl('slr', filenames)]
   output <- lapply(paste0('data/workingData/gtwr_coef/', filenames_slr), read.csv) %>% 
      do.call(rbind, .)
   poll <- strsplit(csv_name, '_')[[1]][2]
   if(poll=='PM2.5'){
      output <- output %>% filter(!(year%in%(2000:2003)&(fold==4)))
   }
   output
}) %>% do.call(rbind, .)


lapply(csv_names, function(csv_name){
   poll <- strsplit(csv_name, '_')[[1]][2]
   if(poll=='PM2.5') poll <- 'PM25'
   filenames <- list.files('data/workingData/gtwr_coef/', csv_name)
   filenames <- filenames[!grepl('slr', filenames)]
   filenames <- filenames[!grepl('.tif.aux.xml', filenames)]
   lapply(paste0('data/workingData/gtwr_coef/', filenames), stack) %>% 
      do.call(stack, .) %>% writeRaster(., paste0('data/processed/GTWR00-19_', poll, '_5fold.tiff'))
})

write.csv(all_coefslr, 'data/processed/combined/SLR00-19_5CV.csv', row.names = F)

