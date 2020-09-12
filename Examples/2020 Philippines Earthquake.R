# options(scipen=999)
# 2020 Philippines Earthquake
source("Ref.R")
DP_Year <- 2019

#Earthquake Radius and Center
library(dismo)
DP_Radius <- 50000
DF_Points <- data.frame(lon = c(124.107),
                        lat = c(12.010))

#Color Schemes
library(RColorBrewer)
Color_Palette_Top <-
  colorRampPalette(c(rgb(1, 1, 0, 0.2)), alpha = TRUE)
Color_Palette_Base <- colorRampPalette(brewer.pal(9, "Reds"))

#Identify Impacted Countries
library(rnaturalearth)
SP_Countries <- ne_countries(scale = 10)
CC_Impact <-
  circles(DF_Points, DP_Radius, lonlat = TRUE, dissolve = FALSE)
SP_Impact <- polygons(CC_Impact)
SP_Impact@proj4string <- SP_Countries@proj4string
library(rgeos)
DL_Countries <-
  lapply(1:nrow(SP_Countries), function(x)
    ifelse(gOverlaps(SP_Countries[x,], SP_Impact), SP_Countries[x,]$iso_a3, NA))
DL_Countries <- DL_Countries[!is.na(DL_Countries[])]
DP_Country_ISO3 <- DL_Countries[[1]]

# GDP Raster
source(Ref.DIR_FUNC_LitPop)
source(Ref.DIR_FUNC_MEF)
Raster_GDP_Proxy_Country <-
  LitPop.Get_GDP_Proxy_Raster(DP_Country_ISO3, DP_Year)
DP_Country_GDP <- MEF.Get_GDP_Value(DP_Country_ISO3, DP_Year)
Raster_GDP_Country <-
  Raster_GDP_Proxy_Country * (DP_Country_GDP$value / 10 ^ 6)
SP_Country <-
  SP_Countries[!is.na(SP_Countries$iso_a3) &
                 SP_Countries$iso_a3 == DP_Country_ISO3,]
SP_States <- ne_states(country = SP_Country$name_en)
DP_Country_Name <- SP_Country$name_en

#States GDP Map with Impacted Area
DL_States <-
  lapply(1:nrow(SP_States), function(x)
    ifelse(gOverlaps(SP_States[x,], SP_Impact), SP_States[x,]$name_en, NA))
DL_States <- DL_States[!is.na(DL_States[])]
SP_States_Impacted <-
  SP_States[SP_States$name_en %in% DL_States,]
SP_States_BBox <-
  bbox2SP(bbox = bbox(SP_States_Impacted),
          proj4string = SP_States_Impacted@proj4string)
DL_States_Core <-
  lapply(1:nrow(SP_States), function(x)
    ifelse(gWithin(SP_States[x,], SP_States_BBox), SP_States[x,]$name_en, NA))
DL_States_Core <- DL_States_Core[!is.na(DL_States_Core[])]
SP_States_Core <-
  SP_States[SP_States$name_en %in% DL_States_Core, ]

plot(Raster_GDP_Country[SP_States_Core, drop = F], col = Color_Palette_Base(100))
plot(SP_States_Core, add = T)
MX_Coordinates <- coordinates(SP_States_Core)
text(x = MX_Coordinates[, 1],
     y = MX_Coordinates[, 2],
     SP_States_Core$name_en,
     pos = 1)
plot(SP_Impact, add = T, col = Color_Palette_Top(1))

# Print Impacted MEF Loss
source(Ref.DIR_FUNC_Output)
Output.Print_Impact(DP_Country_ISO3,
                    DP_Year,
                    Raster_GDP_Proxy_Country,
                    SP_Country,
                    SP_Impact)