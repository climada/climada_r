List_ASTG_Names <- c('N01E103', 'N01E104')
List_Raster_DEM <- c()

library(sp)
for (name in List_ASTG_Names){
  Raster_i <- raster(paste0('./Data/earthdata.nasa.gov/ASTGTMV003_', name, '_dem.tif'))
  List_Raster_DEM <- c(List_Raster_DEM, Raster_i)
}
Raster_DEM <- do.call(merge, List_Raster_DEM)



library(sf)
SF_States <- read_sf("./Data/data.gov.sg/Planning_Area_Census2010.shp")
SF_States <- st_transform(SF_States, st_crs("+proj=longlat +datum=WGS84"))
SP_States <- as_Spatial(SF_States)

SF_Country <- st_union(SF_States$geometry)
SP_Country <- as_Spatial(SF_Country)

plot(SP_Country)
plot(SP_States)



Raster_States <- Raster_DEM[SP_States, drop = F]
plot(Raster_States)

Raster_Country <- Raster_DEM[SP_Country, drop=F]
plot(Raster_Country)
