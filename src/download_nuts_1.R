library(giscoR)
library(sf)
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")

eu_nuts <- gisco_get_nuts(year='2021', epsg='3035', cache_dir='data/temp/', nuts_level = '1',
               resolution = '01', spatialtype = 'RG', country=eu_bnd$CNTR_ID)
eu_nuts <- eu_nuts %>% st_filter(eu_bnd)
plot(eu_nuts[,1])
plot(eu_bnd[,1])
write_sf(eu_nuts, 'data/processed/nuts1_expanse.shp', layer_options = "ENCODING=UTF-8", 
         delete_layer = T)
