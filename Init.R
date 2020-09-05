install.packages("sp")
install.packages("dismo")
install.packages("raster")
install.packages("tmap")
install.packages("rgeos")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rnaturalearthhires")
install.packages("wbstats")
install.packages("R.utils")


source("Ref.R")


### Download and process nightlight files in Lit folder
source(Ref.DIR_FUNC_Lit)
for (year in c(2016, 2012)) #c(1992:2012, 2016))
{
  Raster_Lit <- Lit.Get_Nightlight_Raster(year)
}
plot(Raster_Lit)


### Check if population files exist
# To download pop raster files from below link
# https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11
source(Ref.DIR_FUNC_Pop)
for (year in c(2015, 2020)) #c(2000, 2005, 2010, 2015, 2020))
{
  Raster_Pop <- Pop.Get_Population_Raster(year)
}
plot(Raster_Pop)


### Check if population files exist
source(Ref.DIR_FUNC_LitPop)
for  (year in c(1992:2012, 2016))
{
  Raster_LitPop <- LitPop.Get_GDP_Raster("JPN", year) 
}
plot(Raster_LitPop)


### Update GDP data file in the MEF folder
# DP_Year <- 2019
# source(Ref.DIR_FUNC_MEF)
# DF_Countries <- MEF.Get_Country_Info_Fr_WB()
# for (iso3 in DF_Countries$iso3c)
# {
#   try({
#     print(iso3)
#     print(MEF.Get_GDP_Value(iso3, DP_Year))
#   })
# }


### Check if population files exist
# source(Ref.DIR_FUNC_SPI)
# for  (year in c(1969, 1979, 1989, 1999, 2009, 2018))
# {
#   Raster_SPI <- SPI.Get_Precipitation_Raster(year, 7) 
# }
# plot(Raster_SPI)

