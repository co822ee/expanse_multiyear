source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# https://code.earthengine.google.com/64749d4d5f2d549a989f3b95176dd44a?noload=1
met <- read.csv("../EXPANSE_predictor/data/raw/gee/met_00_19.csv")  # Met data from GEE
met %>% names()
met <- met %>% 
   dplyr::select(-".geo", -"system.index") %>%
   rename(station_european_code=Station)
met_yr <- met %>% names() %>% 
   strsplit(., "_") %>% 
   lapply(.,  `[[`, 2) %>% 
   unlist() %>% as.numeric()
met_yr[is.na(met_yr)] <- 0
yr_target
if(all(yr_target%in%met_yr)){
   # no2_e_sub <- no2_e_all %>% filter(year%in%yr_target)
   if(length(yr_target)==1){
      # Single year
      # yr_target <- 2000
      met_target <- met[, met_yr==yr_target]
      # names(met_target) <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist()
      met_target$station_european_code <- met$station_european_code
      no2_e_sub <- inner_join(no2_e_sub, met_target, by="station_european_code")
      
   }else{
      # Multiple years
      # yr_target <- 2008:2010
      met_target <- met[, met_yr%in%yr_target]
      met_var <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist %>% unique
      met_str <- lapply(met_var, function(met_str) paste0(met_str, "_", no2_e_sub$year))
      no2_e_sub_s <- inner_join(no2_e_sub, met, "station_european_code")
      met_target <- sapply(seq_along(met_str), 
                    function(str_i){
                       sapply(seq_along(met_str[[str_i]]), function(i) no2_e_sub_s[i, met_str[[str_i]][i] ])
                    }
                    ) %>% as.data.frame()
      names(met_target) <- met_var
      # identical(no2_e_sub_s$station_european_code, no2_e_sub$station_european_code)
      no2_e_sub <- cbind(no2_e_sub, met_target)
      
   }
   
   
   
}

if(all(paste0("omi_", yr_target)%in%names(no2_e_sub))){
   # no_omi_year_i <- which(!(paste0("omi_", no2_e_sub$year)%in%names(no2_e_sub)))
   omi_str <- paste0("omi_", no2_e_sub$year)
   # Assign the omi values for each year
   omi <- sapply(seq_along(omi_str), function(i) no2_e_sub[i, omi_str[i]])
   no2_e_sub <- no2_e_sub %>% dplyr::select(-matches("omi"))
   no2_e_sub$omi <- omi
}else{
   no2_e_sub <- no2_e_sub %>% dplyr::select(-matches("omi"))
}