# data_df <- no2_all
# strt_group=c("sta_code", "n_obs") # multiple years
# strt_group=c('type_of_st', 'zoneID') #Single year
# strt_group=c('sta_type', 'zoneID') #Single year
# nfold=5
# This needs to be reset to the original CreateSpacetimeFolds function
# Cause the data from the same station from different years is included in both training data test data.
create_fold <- function(data_df, seed, strt_group=c("sta_code", "n_obs"),
                         nfold=5){
   # data_df: the dataset you want to split for training and testing
   # seed: 123
   # strt_group: groups for stratification
   
   yrs <- unique(data_df$year)
   # data_df <- subset_df_yrs(no2_e_all, yrs)
   # print(paste0("year: ", unique(data_df$year)))
   #f# subset cross-validation data (n-fold cross-validation)
   #f# stratified by station types, climate zones for single years
   #f# leave-location-out cv for multiple-year modelling
   set.seed(seed)
   data_df$index <- 1:nrow(data_df)
   
   
   index_tmp <- vector("list", length=nfold)
     
   
   for(i_fold in seq_along(index_tmp)){
      # print(paste0("fold: ", i_fold))
      if(i_fold==1){
         train_sub <- stratified(data_df, strt_group, 1-(1/nfold))
         test_sub <- data_df[-train_sub$index, ]
         index_tmp[[i_fold]] <- test_sub$index
         # nrow(data_df) %>% print()
         fold_ratio_left <- 1-(1/nfold)
         index_used <- test_sub$index
         length(index_used)
      }else{
         if(i_fold!=nfold){
            # data_df[-(index_used),]$index%in%train_sub$index %>% all()
            # test_sub2 <- stratified(train_sub, strt_group, 0.2/0.8)
            fold_ratio <- (1/nfold)/fold_ratio_left
            
            test_sub2 <- stratified(data_df[-(index_used),], strt_group, fold_ratio)
            index_used <- c(test_sub2$index, index_used)
            train_sub2 <- data_df[-index_used, ]
            index_tmp[[i_fold]] <- test_sub2$index
            
            fold_ratio_left <- 1-(rep(1/nfold, i_fold) %>% sum)
            
         }else{
            test_sub2 <- data_df[-(index_used),]
            index_tmp[[i_fold]] <- test_sub2$index
         }
         
         any(duplicated(index_used))
         length(index_used)
         
         # nrow(test_sub) %>% print()
         # nrow(test_sub2)%>% print()
         # (nrow(test_sub2)/nrow(data_df))%>% print()
         # nrow(train_sub)%>% print()
         # nrow(train_sub2)%>% print()
         # (nrow(train_sub2)/nrow(data_df)) %>% print()
         
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
   data_df <- inner_join(data_df, index_df, by="index")
   
   # return data_df
   return(data_df)
}

# Multiple years
# data_df$station_european_code %>% duplicated() %>% any
#5-fold Cross-validation 
# # Multiple years
# # Method 1: (easier to use)
# strt_group="station_european_code"
# # only leave location out 
# folds=CreateSpacetimeFolds(
#    data_df,
#    spacevar = multiyr_vargroup,     # leave location out
#    timevar = NA,
#    k = nfold,
#    class = NA,
#    seed = seed
# )
# # train_sub <- data_df[folds$index[[1]], ]
# # test_sub <- data_df[folds$indexOut[[1]], ] # The indices in each fold are unique
# index_df <- lapply(seq_along(folds$indexOut), function(fold_i){
#    data.frame(index=folds$indexOut[[fold_i]], nfold=fold_i)
# })
# index_df <- do.call(rbind, index_df)
# data_df <- inner_join(data_df, index_df, by="index")

