require(raster)

source('Ref.R')

DEM.Class <- 'DEM'
DEM.SITE <-
  'https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/ice_surface/grid_registered/georeferenced_tiff/ETOPO1_Ice_g_geotiff.zip'
#DEM.SITE <- 'https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/georeferenced_tiff/ETOPO1_Bed_g_geotiff.zip'

DEM.Get_DEM_Raster <- function(area = NULL)
{
  DP_File <- paste0(Ref.DIR_DATA_DEM, '/ETOPO1_Ice_g_geotiff.tif')
  #DP_File <- paste0(Ref.DIR_DATA_DEM, '/ETOPO1_Bed_g_geotiff.tif')
  
  if (!file.exists(DP_File))
  {
    DP_Path <- paste0(Ref.DIR_DATA_DEM, '/ETOPO1_Ice_g_geotiff.zip')
    #DP_Path <- paste0(Ref.DIR_DATA_DEM, '/ETOPO1_Bed_g_geotiff.zip')
    if (!file.exists(DP_Path))
    {
      download.file(DEM.SITE,
                    destfile = DP_Path,
                    method = "libcurl",
                    mode = "wb")
    }
    unzip(DP_Path, exdir = Ref.DIR_DATA_DEM)
  }
  
  Raster_DEM <-
    raster(DP_File, ext = extent(-180, 180, -90, 90), crs = '+proj=longlat +datum=WGS84 +no_defs')
  
  if (is.null(area)) {
    print(paste0(DEM.Class, ": Returning global DEM raster"))
    return(Raster_DEM)
  } else{
    print(paste0(DEM.Class, ": Returning regional DEM raster"))
    return(Raster_DEM[area, drop = F])
  }
}
