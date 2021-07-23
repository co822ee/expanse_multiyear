create_regressionGrid <- function(extent_sf, cellsize, crs_str){
   #'@title Output
   #'@description Output 
   #'@param extent_sf the sf object for defining the boundary of the regression grid 
   #'@param cellsize the regression grid cell size  (unit: meter)
   #'@param crs_str the crs string for the spatial points
   #'@return data frame object with gtwr as the GTWR predictions
   xmin <- extent(extent_sf)[1]
   ymin <- extent(extent_sf)[3]
   xmax <- extent(extent_sf)[2]
   ymax <- extent(extent_sf)[4]
   
   
   grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                    c(cellsize,cellsize),
                                    c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)),
                       proj4string = crs_str)
   grd2
}