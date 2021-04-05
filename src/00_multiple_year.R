# This script run the three models for multiple single years or multiple years
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
source("../EXPANSE_algorithm/scr/o_00_00_read_data.R")
# Whether to tune RF
tuneRF_b = F
# Multiple single years
csv_names <- paste0('o_', "06-10")   #2008:2012
years <- list(c(2006:2010))
yr_i <- 1
csv_name <- csv_names[yr_i]
print("********************************************")
print(csv_name)
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
# numbers of observations for every year
yrs <- unique(no2_e_09_11$year)
poll_tbl <- with(no2_e_09_11, table(sta_code, year))
temporal_avail <- apply(poll_tbl, 1, sum)
str(temporal_avail)
temporal_avail <- data.frame(sta_code=names(temporal_avail),
                             n_obs=as.numeric(temporal_avail))
no2_all <- inner_join(no2_e_09_11, temporal_avail, by="sta_code")
no2_all %>% names
# All year available
# temporal_avail[temporal_avail==length(yrs)] %>% names
source("src/00_fun_create_fold.R")
# Cause the data from the same station from different years is included in both training data test data.
data_all1 <- create_fold(no2_all, seed, strt_group=c("sta_code", "n_obs", "sta_type", "zoneID"), 
                         nfold = 5)
data_all1 %>% names()
# (data_all1 %>% dplyr::filter(nfold==1) %>% dplyr::filter(n_obs==5, year==2006) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==2) %>% dplyr::filter(n_obs==5, year==2006) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==1) %>% dplyr::filter(n_obs==4, year==2006) %>% nrow)/(data_all1 %>% nrow)
# (data_all1 %>% dplyr::filter(nfold==2) %>% dplyr::filter(n_obs==4, year==2006) %>% nrow)/(data_all1 %>% nrow)
