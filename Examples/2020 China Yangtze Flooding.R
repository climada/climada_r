# options(scipen=999)
# Color Setting
library(RColorBrewer)
#brewer.pal.info
# Color for GDP Distribution Map
Color_Palette_Assets <- colorRampPalette(brewer.pal(9,"Reds"))
Color_Palette_Rivers <- colorRampPalette(c("deepskyblue"))
Color_Palette_MainRv <- colorRampPalette(c("blue"))
Color_Palette_Impact <- colorRampPalette(c(rgb(1,1,0,0.2)), alpha = TRUE)
Color_Palette_Height <- 
  colorRampPalette(
    c(
      "darkolivegreen3",
      "palegreen4",
      "cornsilk",
      "cornsilk2",
      "cornsilk3",
      "gray80",
      "gray70",
      "gray60",
      "gray50",
      "gray40",
      "snow3",
      "snow2",
      "snow1",
      "snow"
    )
  )


# China Yangtze Flooding
########################
# Flood Impacted Margin (km)
DP_Margin <- 15
# Year of Impact
DP_Year <- 2019
# Country
DP_Country_ISO3 <- 'CHN'
# Impacted States
VT_States_Core <- c("Anhui", "Hubei", "Jiangsu", "Zhejiang", "Jiangxi", "Shanghai", "Hunan")
########################

# National and States Boundaries
library(rnaturalearth)
SP_Countries <- ne_countries()
SP_Country <- SP_Countries[!is.na(SP_Countries$iso_a3) & SP_Countries$iso_a3 == DP_Country_ISO3, ]
SP_States <- ne_states(country = SP_Country$name)
SP_States_Core <- SP_States[SP_States$name %in% VT_States_Core,]

# River Shapes
library(tmap)
data("rivers")
SP_Rivers <- as(rivers, "Spatial")
SP_Rivers_Country <- intersect(SP_Rivers, SP_Country)

# Yangtze River Area Flooding
# https://www.reuters.com/article/us-china-weather-floods/red-alerts-in-china-as-floods-maroon-equipment-to-fight-coronavirus-idUSKCN24I0G0
SP_Rivers_Yangtze <- SP_Rivers_Country[SP_Rivers_Country$name.1 %in% c("Yangtze","Chang Jiang"),]
SP_Impact <- buffer(SP_Rivers_Yangtze, width=DP_Margin/100)

# Altitude Map
Raster_Alt_Country <- getData('alt', country=DP_Country_ISO3, mask=TRUE, path = Ref.DIR_DATA_Ref)

# GDP Distribution Map
source("Ref.R")
source(Ref.DIR_FUNC_LitPop)
Raster_GDP_Proxy_Country <- LitPop.Get_GDP_Proxy_Raster(DP_Country_ISO3, DP_Year)
source(Ref.DIR_FUNC_MEF)
DF_Country_GDP <- MEF.Get_GDP_Value(DP_Country_ISO3, DP_Year)
Raster_GDP_Country <- Raster_GDP_Proxy_Country * (DF_Country_GDP$value / 10 ^ 6)



#A. Plot Country Contour
########################
plot(Raster_Alt_Country,  col=Color_Palette_Height(255))
# Plot States Boundaries
plot(SP_States, add=T)
# Plot Rivers
plot(SP_Rivers_Country, add=T, lwd=SP_Rivers_Country$strokelwd/5, col=Color_Palette_Rivers(1))
plot(SP_Rivers_Yangtze, add=T, lwd=SP_Rivers_Yangtze$strokelwd/5, col=Color_Palette_MainRv(1))
# Label State Names
# MX_Coordinates <- coordinates(SP_States)
# text(x=MX_Coordinates[,1],
#      y=MX_Coordinates[,2],
#      SP_States$name,
#      cex = 0.55)
########################



#B. Plot GDP Density Map
########################
plot(Raster_GDP_Country[SP_States_Core, drop=F], col=Color_Palette_Assets(100))
# Plot States Boundaries
plot(SP_States_Core, add=T)
# Plot Rivers
plot(SP_Rivers_Country, add=T, lwd=SP_Rivers_Country$strokelwd/5, col=Color_Palette_Rivers(1))
plot(SP_Rivers_Yangtze, add=T, lwd=SP_Rivers_Yangtze$strokelwd/5, col=Color_Palette_MainRv(1))
# Label State Names
MX_Coordinates <- coordinates(SP_States_Core)
text(x=MX_Coordinates[,1],
     y=MX_Coordinates[,2],
     SP_States_Core$name,
     cex = 1.0)
# Plot Impacted Area
plot(SP_Impact, add=T, col=Color_Palette_Impact(1))
########################



#C. Estimate the Impacts
########################
source(Ref.DIR_FUNC_Output)
Output.Print_Impact(DP_Country_ISO3,
                    DP_Year,
                    Raster_GDP_Proxy_Country,
                    SP_Country,
                    SP_Impact)
########################

