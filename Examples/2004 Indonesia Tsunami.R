# options(scipen=999)
# 2004 Indonesia Tsunami
source("Ref.R")

# Year of Impact
DP_Year <- 2004

# Tsunami Impact Radius
DP_Radius <- 200000

# Earthquake Center
library(dismo)
DF_Points <- data.frame(lon = c(95.982),
                        lat = c(3.295))

# Country
DP_Country_ISO3 <- 'IDN'

# GDP Distribution Map
source(Ref.DIR_FUNC_LitPop)
Raster_GDP_Proxy_Country <-
  LitPop.Get_GDP_Proxy_Raster(DP_Country_ISO3, DP_Year)
source(Ref.DIR_FUNC_MEF)
DF_Country_GDP <- MEF.Get_GDP_Value(DP_Country_ISO3, DP_Year)
Raster_GDP_Country <-
  Raster_GDP_Proxy_Country * (DF_Country_GDP$value / 10 ^ 6)

# National and States Boundaries
library(rnaturalearth)
SP_Countries <- ne_countries()
SP_Country <-
  SP_Countries[!is.na(SP_Countries$iso_a3) &
                 SP_Countries$iso_a3 == DP_Country_ISO3,]
SP_States <- ne_states(country = SP_Country$name)

# Impacted States
VT_States_Core <-
  c(
    "Aceh",
    "North Sumatra Province",
    "West Sumatra",
    "Riau Province",
    "Bengkulu Province",
    "Jambi Province",
    "South Sumatra",
    "Lampung Province"
  )
SP_States_Core <- SP_States[SP_States$name_en %in% VT_States_Core, ]

library(RColorBrewer)
# Purple Color for GDP Distribution Map
Color_Palette <- colorRampPalette(brewer.pal(9, "Reds"))
# Red Color for Impacted Area
Color_Palette_Impact <-
  colorRampPalette(c(rgb(1, 1, 0, 0.2)), alpha = TRUE)

# Plot GDP
plot(Raster_GDP_Country[SP_States_Core, drop = F], col = Color_Palette(100))
# Plot States Boundaries
plot(SP_States_Core, add = T)
# Label State Names
MX_Coordinates <- coordinates(SP_States_Core)
text(
  x = MX_Coordinates[, 1],
  y = MX_Coordinates[, 2],
  SP_States_Core$name_en,
  cex = 0.55,
  pos = 3
)
# Plot Impacted Area
CC_Impact <-
  circles(DF_Points, DP_Radius, lonlat = TRUE, dissolve = FALSE)
plot(CC_Impact, col = Color_Palette_Impact(1), add = T)


# Extract Impacted Area from GDP Maps & Population Maps
SP_Impact <- polygons(CC_Impact)
source(Ref.DIR_FUNC_Output)
Output.Print_Impact(DP_Country_ISO3,
                    DP_Year,
                    Raster_GDP_Proxy_Country,
                    SP_Country,
                    SP_Impact)
