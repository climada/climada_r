require(stringr)
require(R.utils)
require(raster)

source('Ref.R')

Lit.Class <- 'Lit'

Lit.FILES_NASA_BM <-
  data.frame(
    A = c(
      'BlackMarble_*_A1_geo_gray.tif',
      'BlackMarble_*_A2_geo_gray.tif'
    ),
    B = c(
      'BlackMarble_*_B1_geo_gray.tif',
      'BlackMarble_*_B2_geo_gray.tif'
    ),
    C = c(
      'BlackMarble_*_C1_geo_gray.tif',
      'BlackMarble_*_C2_geo_gray.tif'
    ),
    D = c(
      'BlackMarble_*_D1_geo_gray.tif',
      'BlackMarble_*_D2_geo_gray.tif'
    )
  )

Lit.SITE_NASA_BM <-
  'https://www.nasa.gov/specials/blackmarble/*/tiles/georeferrenced/'
Lit.FILE_NASA_BM <- 'BlackMarble_*.tif'

Lit.SITE_NOAA_SL <-
  "https://ngdc.noaa.gov/eog/data/web_data/v4composites/"
Lit.FILE_NOAA_SL <- 'StableLight_*.tif'



#Extract nightlight raster as a whole or by parts
#year: reference year
#area: raster or polygon objects to cut a smaller part
Lit.Get_Nightlight_Raster <- function(year = 2016, area = NULL)
{
  print(paste0(Lit.Class, ": Extracting nightlight raster for ", year))
  if (year > 2011)
    Raster_Lit <- Lit.Get_NASA_BM_Raster(year)
  else
    Raster_Lit <- Lit.Get_NOAA_SL_Raster(year)
  
  if (is.null(area)) {
    print(paste0(Lit.Class, ": Returning global nightlight raster"))
    return(Raster_Lit)
  } else{
    print(paste0(Lit.Class, ": Returning regional nightlight raster"))
    return(Raster_Lit[area, drop = F])
  }
}

Lit.Get_NASA_BM_Raster <- function(year = 2016)
{
  year <- ifelse (year >= 2016, 2016, 2012)
  print(paste0(Lit.Class, ": Extracting NASA nightlight raster for ", year))
  Path_NASA_BM <-
    paste0(Ref.DIR_DATA_Lit, sub("*", year, Lit.FILE_NASA_BM, fixed = T))
  if (file.exists(Path_NASA_BM)) {
    Raster_Lit <- raster(Path_NASA_BM, ext = extent(-180, 180, -90, 90))
  } else{
    Raster_Lit <- Lit.Download_NASA_BM_Files(year)
  }
  return(Raster_Lit)
}

Lit.Get_NOAA_SL_Raster <- function(year = 2013)
{
  if (year > 2013)
    year <- 2013
  if (year < 1992)
    year <- 1992
  print(paste0(Lit.Class, ": Extracting NOAA nightlight raster for ", year))
  Path_NOAA_SL <-
    paste0(Ref.DIR_DATA_Lit, sub("*", year, Lit.FILE_NOAA_SL, fixed = T))
  if (!file.exists(Path_NOAA_SL)) {
    Lit.Download_NOAA_SL_Files(year)
  }
  Raster_Lit <-
    raster(Path_NOAA_SL, ext = extent(-180, 180, -65, 75))
  return(Raster_Lit)
}



#Download and process NOAA nightlight files
Lit.Download_NOAA_SL_Files <- function(year = 2013)
{
  if (year > 2013)
    year <- 2013
  if (year < 1992)
    year <- 1992
  
  DP_File <- DP_Path <- ''
  for (i in c(18:10))
  {
    DP_File <- paste0("F", i, year)
    DP_Path <- paste0(Ref.DIR_DATA_Lit, DP_File, '.v4.tar')
    DP_Url <- paste0(Lit.SITE_NOAA_SL, DP_File, '.v4.tar')
    
    try({
      if (!file.exists(DP_Path))
      {
        download.file(DP_Url,
                      destfile = DP_Path,
                      method = "libcurl",
                      mode = "wb")
        print(paste0(Lit.Class, ": Downloaded NOAA nightlight files for ", year))
      }
      break
    })
  }
  
  print(paste0(Lit.Class, ": Untaring NOAA nightlight files from ", DP_Path))
  VT_Dest <- untar(DP_Path, list = T)
  VT_Dest <-
    str_extract(
      VT_Dest,
      paste0(
        "(",
        DP_File,
        ".v4){1}[:alpha:]{1}(_web.stable_lights.avg_vis.tif.gz){1}"
      )
    )
  DP_Dest <- VT_Dest[!is.na(VT_Dest)][1]
  untar(DP_Path, exdir = Ref.DIR_DATA_Lit, files = c(DP_Dest))
  print(paste0(Lit.Class, ": Unziping NOAA nightlight files from ", DP_Dest))
  gunzip(
    filename = paste0(Ref.DIR_DATA_Lit,
                      DP_Dest),
    destname = paste0(Ref.DIR_DATA_Lit,
                      sub("*", year, Lit.FILE_NOAA_SL, fixed = T)),
    overwrite = T
  )
}



#Download and process NASA nightlight files
Lit.Download_NASA_BM_Files <- function(year = 2016)
{
  year <- ifelse (year >= 2016, 2016, 2012)
  
  if (!Lit.Existing_NASA_BM_Files(year))
  {
    print(paste0(Lit.Class, ": Downloading NASA nightlight files for ", year))
    for (i in c(1:2)) {
      for (j in c(1:4)) {
        DP_Url <-
          gsub("*",
               year,
               paste0(Lit.SITE_NASA_BM, Lit.FILES_NASA_BM[i, j]),
               fixed = T)
        DP_Path <-
          sub("*",
              year,
              paste0(Ref.DIR_DATA_Lit, Lit.FILES_NASA_BM[i, j]),
              fixed = T)
        download.file(DP_Url,
                      destfile = DP_Path,
                      method = "libcurl",
                      mode = "wb")
      }
    }
    print(paste0(Lit.Class, ": Downloaded NASA nightlight files for ", year))
  }
  
  print(paste0(Lit.Class, ": Merging NASA nightlight segment files for ", year))
  List_Raster_Lit <- c()
  for (i in c(1:4)) {
    Raster_1 <-
      raster(paste0(
        Ref.DIR_DATA_Lit,
        sub('*', year, Lit.FILES_NASA_BM[1, i], fixed = T)
      )
      ,
      ext = extent(-180 + (i - 1) * 90, -90 + (i - 1) * 90, 0, 90))
    Raster_1[is.na(Raster_1[])] <- 0
    List_Raster_Lit <- c(List_Raster_Lit, Raster_1)
    Raster_2 <-
      raster(paste0(
        Ref.DIR_DATA_Lit,
        sub('*', year, Lit.FILES_NASA_BM[2, i], fixed = T)
      )
      ,
      ext = extent(-180 + (i - 1) * 90, -90 + (i - 1) * 90, -90, 0))
    Raster_2[is.na(Raster_2[])] <- 0
    List_Raster_Lit <- c(List_Raster_Lit, Raster_2)
  }
  Path_NASA_BM <-
    paste0(Ref.DIR_DATA_Lit, sub("*", year, Lit.FILE_NASA_BM, fixed = T))
  List_Raster_Lit$filename <- Path_NASA_BM
  List_Raster_Lit$overwrite <- T
  print(paste0(Lit.Class, ": Saving merged global nightlight file for ", year))
  Raster_Lit <- do.call(merge, List_Raster_Lit)
  return(Raster_Lit)
}



#Check if NASA nightlight files already exist
Lit.Existing_NASA_BM_Files <- function(year = 2016)
{
  for (i in c(1:2)) {
    for (j in c(1:4)) {
      DP_File <- sub("*", year, Lit.FILES_NASA_BM[i, j], fixed = T)
      DP_Path <- paste0(Ref.DIR_DATA_Lit, DP_File)
      if (!file.exists(DP_Path))
        return(F)
    }
  }
  return(T)
}


