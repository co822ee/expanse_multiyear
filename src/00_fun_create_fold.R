# data_df <- no2_all
# strt_group=c("n_obs", "sta_type", "zoneID") # multiple years
# strt_group=c('type_of_st', 'zoneID') #Single year
# strt_group=c('sta_type', 'zoneID') #Single year
# nfold=5
# This needs to be reset to the original CreateSpacetimeFolds function
# Cause the data from the same station from different years is included in both training data test data.
create_fold <- function(data_df, seed, strt_group, multiyear_group = c("sta_code", "year"),
                         nfold=5){
   # data_df: the dataset you want to split for training and testing
   # seed: 123
   # strt_group: groups for stratification
   # multiyear_group: groups for identifying "unique" stations for multiple-year modelling
   #f# subset cross-validation data (n-fold cross-validation)
   #f# stratified by station types, climate zones for single-year modelling
   #f# stratified by station types, climate zones
   #                   and availability over time for multiple-year modelling
   
   #f# leave-location-out cv for multiple-year modelling
   # numbers of observations for every year
   yrs <- unique(data_df$year)
   
   if(length(yrs)!=1){
      # Obtain the number of observations for each monitoring station over time
      # poll_tbl <- with(data_df, table(sta_code, year)) 
      poll_tbl <- table(data_df[, multiyear_group])
      temporal_avail <- apply(poll_tbl, 1, sum)
      temporal_avail <- data.frame(sta_code=names(temporal_avail),
                                   n_obs=as.numeric(temporal_avail))
      temporal_avail <- inner_join(temporal_avail,
                                   data_df, 
                                   by="sta_code")
      temporal_avail <- temporal_avail[!duplicated(temporal_avail$sta_code),]  
      # Give the unique monitoring stations so that observations from each station can only be either training or test data.
      data_df2 <- temporal_avail
   }else{
      data_df2 <- data_df
   }
   
   set.seed(seed)
   data_df2$index <- 1:nrow(data_df2)
   
   index_tmp <- vector("list", length=nfold)
   
   for(i_fold in seq_along(index_tmp)){
      # print(paste0("fold: ", i_fold))
      if(i_fold==1){
         train_sub <- stratified(data_df2, strt_group, 1-(1/nfold))
         test_sub <- data_df2[-train_sub$index, ]
         index_tmp[[i_fold]] <- test_sub$index
         # nrow(data_df2) %>% print()
         fold_ratio_left <- 1-(1/nfold)
         index_used <- test_sub$index
         length(index_used)
      }else{
         if(i_fold!=nfold){
            # data_df2[-(index_used),]$index%in%train_sub$index %>% all()
            # test_sub2 <- stratified(train_sub, strt_group, 0.2/0.8)
            fold_ratio <- (1/nfold)/fold_ratio_left
            
            test_sub2 <- stratified(data_df2[-(index_used),], strt_group, fold_ratio)
            index_used <- c(test_sub2$index, index_used)
            train_sub2 <- data_df2[-index_used, ]
            index_tmp[[i_fold]] <- test_sub2$index
            
            fold_ratio_left <- 1-(rep(1/nfold, i_fold) %>% sum)
            
         }else{
            test_sub2 <- data_df2[-(index_used),]
            index_tmp[[i_fold]] <- test_sub2$index
         }
         
         any(duplicated(index_used))
         length(index_used)
         
         # nrow(test_sub) %>% print()
         # nrow(test_sub2)%>% print()
         # (nrow(test_sub2)/nrow(data_df2))%>% print()
         # nrow(train_sub)%>% print()
         # nrow(train_sub2)%>% print()
         # (nrow(train_sub2)/nrow(data_df2)) %>% print()
         
      } 
   }
   # Indices in all folds are unique
   # index_tmp %>% unlist %>% duplicated %>% any()
   sapply(index_tmp, length)
   index_df <- lapply(seq_along(index_tmp), function(index_group){
      data.frame(index=index_tmp[[index_group]], nfold=index_group)
   })
   sapply(index_df, dim)
   index_df <- do.call(rbind, index_df)
   data_df2 <- inner_join(data_df2, index_df, by="index")
   if(length(yrs)!=1){
      # Combine with the original multi-year data
      data_df2 <- inner_join(data_df, data_df2 %>% dplyr::select(sta_code, nfold, n_obs), by="sta_code")
      data_df2$index <- 1:nrow(data_df2)
   }
   # return data_df2
   return(data_df2)
}

# Multiple years
# data_df2$station_european_code %>% duplicated() %>% any
#5-fold Cross-validation 
# # Multiple years
# # Method 1: (easier to use)
# strt_group="station_european_code"
# # only leave location out 
# folds=CreateSpacetimeFolds(
#    data_df2,
#    spacevar = multiyr_vargroup,     # leave location out
#    timevar = NA,
#    k = nfold,
#    class = NA,
#    seed = seed
# )
# # train_sub <- data_df2[folds$index[[1]], ]
# # test_sub <- data_df2[folds$indexOut[[1]], ] # The indices in each fold are unique
# index_df <- lapply(seq_along(folds$indexOut), function(fold_i){
#    data.frame(index=folds$indexOut[[fold_i]], nfold=fold_i)
# })
# index_df <- do.call(rbind, index_df)
# data_df2 <- inner_join(data_df2, index_df, by="index")

