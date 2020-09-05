require(stringr)
require(raster)

source('Ref.R')

SPI.Class <- 'SPI'
SPI.SITE <- 'http://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_*.zip'
SPI.Years <- c(1969, 1979, 1989, 1999, 2009, 2018)



SPI.Get_Precipitation_Raster <- function(year=2018, month=12, area = NULL)
{
  if(month < 1) month <- 1
  if(month > 12) month <- 12
  if(year > 2018) year <- 2018
  DP_Year <- min(SPI.Years[SPI.Years>=year])
  
  DP_File <- paste0(Ref.DIR_DATA_SPI, '/wc2.1_2.5m_prec_', year, '-', formatC(month, width=2, flag="0"),'.tif')
  if(!file.exists(DP_File))
  {
    DP_Period <- paste0(floor(DP_Year/10)*10, '-', DP_Year)
    DP_Url <- sub('*', DP_Period, SPI.SITE, fixed = T)
    DP_Path <- paste0(Ref.DIR_DATA_SPI, '/wc2.1_2.5m_prec_', DP_Period, '.zip')
    if(!file.exists(DP_Path))
    {
      download.file(DP_Url,
                    destfile = DP_Path,
                    method = "libcurl",
                    mode = "wb")
    }
    unzip(DP_Path, exdir = Ref.DIR_DATA_SPI)
  }
  
  Raster_SPI <- raster(DP_File, ext = extent(-180, 180, -90, 90))
  if (is.null(area)) {
    print(paste0(SPI.Class, ": Returning global Precipitation raster"))
    return(Raster_SPI)
  } else{
    print(paste0(SPI.Class, ": Returning regional Precipitation raster"))
    return(Raster_SPI[area, drop = F])
  }
}


