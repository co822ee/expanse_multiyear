# Try gtwr
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(GWmodel)
library(sp)
# Whether to tune RF
tuneRF_b = F
# Multiple single years
csv_names <- paste0('run2_',c('08-10', '09-11', '10-12', 
                              '08-12', '06-12', '05-12', '04-12'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 
              2008:2012, 2006:2012, 2005:2012, 2004:2012)
nfold <- 5
yr_i <- 1
csv_name <- csv_names[yr_i]
print("********************************************")
print(csv_name)

source("../expanse_multiyear/src/00_fun_read_data.R")

csv_name <- csv_names[yr_i]
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
poll_tbl <- with(no2_e_09_11, table(sta_code, year))
temporal_avail <- apply(poll_tbl, 1, sum)
str(temporal_avail)
temporal_avail <- data.frame(sta_code=names(temporal_avail),
                             n_obs=as.numeric(temporal_avail))
temporal_avail <- inner_join(temporal_avail,
                             no2_e_09_11 %>% dplyr::select(sta_code, sta_type, zoneID), 
                             by="sta_code")
temporal_avail <- temporal_avail[!duplicated(temporal_avail$sta_code),]

# All year available
# temporal_avail[temporal_avail==length(yrs)] %>% names
source("src/00_fun_create_fold.R")
# The stations will only be included in one specific fold.
# sta_split <- create_fold(temporal_avail, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
#                          nfold = 5)
# data_all1 <- inner_join(no2_e_09_11, sta_split %>% dplyr::select(sta_code, nfold, n_obs), by="sta_code")
# data_all1$index <- 1:nrow(data_all1)

train_sub <- inner_join(no2_e_09_11, temporal_avail, by="sta_code")
# Use only stations that are available during the period
train_data <- train_sub[train_sub$n_obs==length(years[[yr_i]]),]
sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                       coords = cbind(train_data[, "xcoord"], train_data[, "ycoord"]),
                                       proj4string = local_crs)
#calibrate bandwidth
bw <- bw.gtwr(obs~pressure+precip+allRoads_400+factor(year), 
              sp_train, as.numeric(sp_train@data$year),
              approach = 'CV', kernel = "exponential", adaptive = T)


