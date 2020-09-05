require(raster)

source('Ref.R')

Pop.Class <- 'Pop'

Pop.FILE_NASA_GPW <- 'gpw_v4_population_count_rev11_*_30_sec.tif'

#Extract population raster as a whole or by parts
#year: reference year
#area: area to extract
Pop.Get_Population_Raster <- function(year = 2020, area = NULL)
{
  if(year >= 2020)
    year <- 2020
  else if(year >= 2015)
    year <- 2015
  else if(year >= 2010)
    year <- 2010
  else if(year >= 2005)
    year <- 2005
  else
    year <- 2000
  
  Path_NASA_GPW <- paste0(Ref.DIR_DATA_Pop, sub("*", year, Pop.FILE_NASA_GPW, fixed = T))
  if(file.exists(Path_NASA_GPW)){
    print(paste0(Pop.Class, ": Extracting population raster from file for ", year))
    Raster_Pop <- raster(Path_NASA_GPW, ext = extent(-180, 180, -90, 90))
  }else{
    stop(paste0(Pop.Class, ": Data file missing, pls download from https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11 for ", year))
    return()
  }
  
  if(is.null(area))
  {
    print(paste0(Pop.Class, ": Returning global population raster for ", year))
    return(Raster_Pop)
  }
  else
  {
    print(paste0(Pop.Class, ": Returning regional population raster for ", year))
    return(Raster_Pop[area, drop = F])
  }
}