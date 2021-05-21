# This script output the observation csv files for multiple single years or multiple years
#
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
target_poll <- 'PM2.5'
source("src/00_fun_read_data.R")
# Whether to tune RF
tuneRF = F



# Multiple years
csv_names <- paste0('run2_',c('08-10', '09-11', '10-12', 
                              '08-12', '06-12', '05-12', '04-12', 
                              paste(rep('0', 10), seq(0, 9, 1) %>% as.character(), sep = ""), 
                              seq(10, 12, 1)))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2005:2012, 2004:2012)
years <- c(years, as.list(seq(2000, 2012)))
multiple <- data.frame()
oneyear <- data.frame()
for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   if(length(years[[yr_i]])>1){
      outputDir <- "data/processed/gee/multiyear/"
   }else{
      outputDir <- "data/processed/gee/singleyear/"
   }
   if(!dir.exists(outputDir)) dir.create(outputDir)
   if(!file.exists(paste0(outputDir, csv_name, '_', target_poll,".csv"))){
      no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
      # data_all <- no2_e_09_11
      print(paste0("year: ", unique(no2_e_09_11$year)))
      
      source("src/00_fun_create_fold.R")
      # The stations will only be included in one specific fold.
      if(length(years[[yr_i]])>1){
         # Multiple years
         data_all1 <- create_fold(no2_e_09_11, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                                  multiyear_group = c("sta_code", "year"),
                                  nfold = 5)
         data_all1 <- data_all1 %>% dplyr::select(zoneID, sta_code, n_obs, nfold, index, 
                                                  year, obs, component_caption,
                                                  xcoord, ycoord)
         data_all1$csvname = csv_name
         multiple <- rbind(multiple, data_all1)
         
      }else{
         if(nrow(no2_e_09_11)>50){
            data_all1 <- create_fold(no2_e_09_11, seed, strt_group=c("sta_type", "zoneID"), 
                                     nfold = 5)
            data_all1 <- data_all1 %>% dplyr::select(zoneID, sta_code, nfold, index, 
                                                     year, obs, component_caption,
                                                     xcoord, ycoord)
            data_all1$csvname = csv_name
            oneyear <- rbind(oneyear, data_all1)
         }
      }
      write.csv(data_all1, paste0(outputDir, csv_name, '_', target_poll,".csv"), row.names=F)
   }
   
} 
# write.csv(oneyear, paste0('data/processed/gee/singleyear_', target_poll, '.csv'), row.names = F)
# write.csv(multiple, paste0('data/processed/gee/multiyear_', target_poll, '.csv'), row.names = F)

library(dplyr)
read.csv(paste0('data/processed/gee/multiyear_', target_poll, '.csv'))$csvname %>% unique
read.csv(paste0('data/processed/gee/singleyear_', target_poll, '.csv'))$csvname %>% unique

table(read.csv(paste0('data/processed/gee/singleyear_', target_poll, '.csv'))[, 'year'])
