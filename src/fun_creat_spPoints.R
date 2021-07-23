creat_spPoints <- function(data_df, xcoord, ycoord, crs_str){
   #'@title Create sp data frame
   #'@description Create spatial data point from data frame
   #'@param data_df data frame
   #'@param xcoord the colname of the x coordinate in the data_df data frame
   #'@param ycoord the colname of the y coordinate in the data_df data frame
   #'@param crs_str the crs string for the spatial points
   #'@return spatial points data frame object
   #'
   sp::SpatialPointsDataFrame(data = data_df, 
                              coords = cbind(data_df[, xcoord], data_df[, ycoord]),
                              proj4string = crs_str)
}