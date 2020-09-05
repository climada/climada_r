# options(scipen=999)
source("Ref.R")

# Year of Impact
DP_Year <- 2019

# Country
DP_Country_ISO3 <- 'AUS'

# Tide Height
DP_Tide_Height_M <- 2

library(RColorBrewer)
# Color for GDP Distribution Map
Color_Palette <- colorRampPalette(brewer.pal(9,"Blues"))
# Color for Impacted Area
Color_Palette_Impact <- colorRampPalette(c(rgb(1,0,0,0.2)), alpha = TRUE)

# National and States Boundaries
library(rnaturalearth)
SP_Countries <- ne_countries(scale = 10)
SP_Country <- SP_Countries[!is.na(SP_Countries$iso_a3) & SP_Countries$iso_a3 == DP_Country_ISO3, ]

# GDP Distribution Map
source(Ref.DIR_FUNC_LitPop)
Raster_GDP_Proxy_Country <- LitPop.Get_GDP_Proxy_Raster(DP_Country_ISO3, DP_Year)
source(Ref.DIR_FUNC_MEF)
DF_Country_GDP <- MEF.Get_GDP_Value(DP_Country_ISO3, DP_Year)
Raster_GDP_Country <- Raster_GDP_Proxy_Country * (DF_Country_GDP$value / 10 ^ 6)

# Population Map
source(Ref.DIR_FUNC_Pop)
Raster_Pop_Country <- Pop.Get_Population_Raster(DP_Year, SP_Country) / 10 ^ 6

# Impacted Area with Altitude < Threshold
Raster_Alt_Country <- getData('alt', country=DP_Country_ISO3, mask=TRUE, path = Ref.DIR_DATA_Ref)
Raster_Alt_Impact <- Raster_Alt_Country[SP_Country, drop=F]
Raster_Alt_Impact[Raster_Alt_Impact >= 0 & Raster_Alt_Impact <= DP_Tide_Height_M] <- 1
Raster_Alt_Impact[Raster_Alt_Impact != 1 ] <- NA

# Alternative DEM
# library(elevatr)
# Raster_Alt_Country <- get_elev_raster(SP_Country, 5, prj=SP_Country@proj4string)
# Raster_Alt_Impact <- Raster_Alt_Country
# Raster_Alt_Impact[Raster_Alt_Impact >= 0 & Raster_Alt_Impact <= DP_Tide_Height_M] <- 1
# Raster_Alt_Impact[Raster_Alt_Impact != 1 ] <- NA

# source(Ref.DIR_FUNC_DEM)
# Raster_Alt_Country <- DEM.Get_DEM_Raster(SP_Country)
# Raster_Alt_Impact <- Raster_Alt_Country
# Raster_Alt_Impact[Raster_Alt_Impact >= 0 & Raster_Alt_Impact <= DP_Tide_Height_M] <- 1
# Raster_Alt_Impact[Raster_Alt_Impact != 1 ] <- NA

# Only for Coastal Area
DP_Coastal_Margin_KM <- 100
SP_Coastlines <- ne_coastline(scale = 10)
SP_Coastline_Country <- buffer(SP_Coastlines[SP_Country,], width=DP_Coastal_Margin_KM/100)
Raster_Impact <- Raster_Alt_Impact[SP_Coastline_Country, drop=F]
SP_Impact <- as(Raster_Impact, 'SpatialPolygonsDataFrame')

# Plot Graphs
Raster_Impact_Resampled <- raster::resample(Raster_Impact, Raster_GDP_Country)
SP_Impact_Resampled <- as(Raster_Impact_Resampled, 'SpatialPolygonsDataFrame')
plot(Raster_GDP_Country, col=Color_Palette(100))
plot(SP_Country, add=T)
plot(SP_Impact_Resampled, add=T, col=Color_Palette_Impact(1), border=Color_Palette_Impact(1))

# Extract Impacted Area from GDP Maps & Population Maps
Raster_Impact_Resampled <- raster::resample(Raster_Impact, Raster_GDP_Country)
Raster_GDP_Impact <-  Raster_Impact_Resampled * Raster_GDP_Country
print(paste0(DP_Country_ISO3, " GDP Impacted: $", cellStats(Raster_GDP_Impact, sum), "mn over Total: $", cellStats(Raster_GDP_Country, sum), "mn"))

Raster_Impact_Resampled <- raster::resample(Raster_Impact, Raster_GDP_Proxy_Country)
Raster_GDP_Proxy_Impact <- Raster_Impact_Resampled * Raster_GDP_Proxy_Country
print(paste0(DP_Country_ISO3, " % GDP Impacted: ", cellStats(Raster_GDP_Proxy_Impact, sum)*100, "%"))

Raster_Impact_Resampled <- raster::resample(Raster_Impact, Raster_Pop_Country)
Raster_Pop_Impacted <- Raster_Impact_Resampled * Raster_Pop_Country
print(paste0(DP_Country_ISO3, " Population Affected: ", cellStats(Raster_Pop_Impacted, sum), "mn"))


