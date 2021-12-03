# This code is to create prediction for the residuals for each year in the year period
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
source("../EXPANSE_algorithm/scr/fun_read_data.R")
# Whether to tune RF
tuneRF = F
# Multiple single years

# Multiple years
# csv_names <- paste0('run2_',c('08-10', '09-11', '10-12', 
#                               '08-12', '06-12', '05-12', '04-12'))   #2008:2012
# years <- list(2008:2010, 2009:2011, 2010:2012, 
#               2008:2012, 2006:2012, 2005:2012, 2004:2012)
# folds <- 1:5
file_dir <- "data/workingData/"
slr_filenames <- list.files(file_dir, "SLR_result_all_run2_")
period_str <- lapply(strsplit(slr_filenames, "_"), `[[`, 5) %>% unlist
period_yr <- strsplit(period_str, "-") %>% lapply(., as.numeric)
slr_names <- paste0(file_dir, slr_filenames)
slr_result <- lapply(slr_names, read.csv)
file_i <- 1
slr_result[[file_i]] %>% head

elapse_no2 %>% names

