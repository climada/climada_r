source("Ref.R")

source(Ref.DIR_FUNC_MEF)
source(Ref.DIR_FUNC_Pop)

Output.Class <- "Output"

Output.Print_Impact <-
  function(country_iso3,
           year,
           raster_gdp_proxy_country,
           sp_country,
           sp_impact)
  {
    DP_DP <- 1
    DP_GDP_Country <-  round(MEF.Get_GDP_Value(country_iso3, year)$value / 10 ^ 6, DP_DP)
    
    Raster_Pop_Country <- Pop.Get_Population_Raster(year, sp_country)
    Raster_Pop_Impacted <- Raster_Pop_Country[sp_impact, drop = F]
    DP_Pop_Affected <-
      round(cellStats(Raster_Pop_Impacted, sum) / 10 ^ 6, DP_DP)
    DP_Pop_Country <-
      round(cellStats(Raster_Pop_Country, sum) / 10 ^ 6, DP_DP)
    DP_Pop_Percent <-
      round(DP_Pop_Affected / DP_Pop_Country * 100, DP_DP)
    print(
      paste0(
        Output.Class,
        ": ",
        country_iso3,
        " affected population: ",
        format(DP_Pop_Affected, nsmall = DP_DP),
        "mn over total: ",
        format(DP_Pop_Country, nsmall = DP_DP),
        "mn, equivalent to ",
        format(DP_Pop_Percent, nsmall = DP_DP),
        "% "
      )
    )
    
    Raster_GDP_Proxy_Impact <-
      raster_gdp_proxy_country[sp_impact, drop = F]
    DP_GDP_Percent <-
      round(cellStats(Raster_GDP_Proxy_Impact, sum) * 100, DP_DP)
    DP_GDP_Impacted <- round(DP_GDP_Percent / 100 * DP_GDP_Country, DP_DP)
    
    print(
      paste0(
        Output.Class,
        ": ",
        country_iso3,
        " impacted GDP: $",
        format(DP_GDP_Impacted, nsmall = DP_DP),
        "mn over total: $",
        format(DP_GDP_Country, nsmall = DP_DP),
        "mn, equivalent to ",
        format(DP_GDP_Percent, nsmall = DP_DP),
        "% "
      )
    )
    
    
  }