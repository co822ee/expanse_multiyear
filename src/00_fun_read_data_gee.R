seed <- 123
local_crs <- CRS("+init=EPSG:3035")
# target_poll = 'NO2'
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
read_data <- function(target_poll, yr){
   # Read in observations and predictor values
   if(target_poll=='PM2.5'){
      df_all <- read.csv('../EXPANSE_predictor/data/raw/gee/pred_pm25All.csv')
   }else if(target_poll=='NO2'){
      df_all <- read.csv('../EXPANSE_predictor/data/raw/gee/pred_no2All.csv') %>% 
         filter(obs<=500)
   }else if(target_poll=='O3'){
      df_all <- read.csv('../EXPANSE_predictor/data/raw/gee/pred_o3All.csv')
   }else if(target_poll=='PM10'){
      df_all <- read.csv('../EXPANSE_predictor/data/raw/gee/pred_pm10All.csv')
   }
   # Read in station metadata
   sta <- read.csv("../EXPANSE_APM/data/processed/all_sta.csv")
   # # There are 3 missing rows and 7 rows with unknown values for sta_type
   # # Remove them
   sta <- sta %>% filter(sta_type!="Unknown", sta_type!="") %>% dplyr::select(-sta_type)
   df_all_subset <- df_all %>% filter(year%in%yr)
   df_all_subset$year <- as.factor(df_all_subset$year)
   df_all_subset$zoneID <- as.factor(df_all_subset$zoneID)
   df_all_subset
   inner_join(df_all_subset, sta, by='sta_code')
}
