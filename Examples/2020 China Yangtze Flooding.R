# options(scipen=999)
# 2020 China Yangtze Flooding
source("Ref.R")

# Year of Impact
DP_Year <- 2019

# Flood Impacted Margin (km)
DP_Margin <- 10

# Country
DP_Country_ISO3 <- 'CHN'

# National and States Boundaries
library(rnaturalearth)
SP_Countries <- ne_countries()
SP_Country <- SP_Countries[!is.na(SP_Countries$iso_a3) & SP_Countries$iso_a3 == DP_Country_ISO3, ]
SP_States <- ne_states(country = SP_Country$name)

# River Shapes
library(tmap)
data("rivers")
SP_Rivers <- as(rivers, "Spatial")
SP_Rivers_Country <- intersect(SP_Rivers, SP_Country)

# Yangtze River Area Flooding
# https://www.reuters.com/article/us-china-weather-floods/red-alerts-in-china-as-floods-maroon-equipment-to-fight-coronavirus-idUSKCN24I0G0
SP_Rivers_Yangtze <- SP_Rivers_Country[SP_Rivers_Country$name.1 == "Yangtze",]
SP_Impact <- buffer(SP_Rivers_Yangtze, width=DP_Margin/100)

# GDP Distribution Map
source(Ref.DIR_FUNC_LitPop)
Raster_GDP_Proxy_Country <- LitPop.Get_GDP_Proxy_Raster(DP_Country_ISO3, DP_Year)
source(Ref.DIR_FUNC_MEF)
DF_Country_GDP <- MEF.Get_GDP_Value(DP_Country_ISO3, DP_Year)
Raster_GDP_Country <- Raster_GDP_Proxy_Country * DF_Country_GDP$value / 10 ^ 6

# Population Map
source(Ref.DIR_FUNC_Pop)
Raster_Pop_Country <- Pop.Get_Population_Raster(DP_Year, SP_Country) / 10 ^ 6

# Impacted States
VT_States_Core <- c("Anhui", "Hubei", "Jiangsu", "Zhejiang", "Jiangxi", "Shanghai", "Hunan")
SP_States_Core <- SP_States[SP_States$name %in% VT_States_Core,]

library(RColorBrewer)
# Purple Color for GDP Distribution Map
Color_Palette <- colorRampPalette(brewer.pal(9,"Reds"))
# Red Color for Impacted Area
Color_Palette_Rivers <- colorRampPalette(c("Blue"))
Color_Palette_Impact <- colorRampPalette(c(rgb(1,1,0,0.2)), alpha = TRUE)

# Plot GDP
plot(Raster_GDP_Country[SP_States_Core, drop=F], col=Color_Palette(100))
# Plot States Boundaries
plot(SP_States_Core, add=T)
# Label State Names
MX_Coordinates <- coordinates(SP_States_Core)
text(x=MX_Coordinates[,1],
    y=MX_Coordinates[,2],
    SP_States_Core$name,
    cex = 0.55, pos = 3)
# Plot Impacted Area
plot(SP_Rivers_Country, add=T, lwd=SP_Rivers_Country$strokelwd/5, col=Color_Palette_Rivers(1))
plot(SP_Impact, add=T, col=Color_Palette_Impact(1))

# Extract Impacted Area from GDP Maps & Population Maps
Raster_GDP_Impact <- Raster_GDP_Country[SP_Impact, drop=F]
print(paste0(DP_Country_ISO3, " GDP Impacted: $", cellStats(Raster_GDP_Impact, sum), "mn over Total: $", cellStats(Raster_GDP_Country, sum), "mn"))

Raster_GDP_Proxy_Impact <- Raster_GDP_Proxy_Country[SP_Impact, drop=F]
print(paste0(DP_Country_ISO3, " % GDP Impacted: ", cellStats(Raster_GDP_Proxy_Impact, sum)*100, "%"))

Raster_Pop_Impacted <- Raster_Pop_Country[SP_Impact, drop=F]
print(paste0(DP_Country_ISO3, " Population Affected: ", cellStats(Raster_Pop_Impacted, sum), "mn"))

