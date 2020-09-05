require(raster)
require(readxl)
require(reshape2)
require(rnaturalearth)

source("Ref.R")

source(Ref.DIR_FUNC_MEF)
source(Ref.DIR_FUNC_Lit)
source(Ref.DIR_FUNC_Pop)


LitPop.Class <- "LitPop"



LitPop.Get_GDP_Raster <-
  function(country_iso3,
           year = 2016,
           params = c(1, 1))
  {
    DF_GDP <- MEF.Get_GDP_Value(country_iso3, year)
    DP_GDP_Value <- DF_GDP$value
    Raster_GDP_Proxy <-
      LitPop.Get_GDP_Proxy_Raster(country_iso3, year, params)
    Raster_GDP <- Raster_GDP_Proxy * DP_GDP_Value
    return(Raster_GDP)
  }



LitPop.Get_LitPop_Raster <-
  function(country_iso3,
           year = 2016,
           params = c(1, 1))
  {
    SP_Countries <- ne_countries(scale = 10)
    SP_Country <- SP_Countries[!is.na(SP_Countries$iso_a3) & SP_Countries$iso_a3 == country_iso3,]
    
    Raster_Lit <- Lit.Get_Nightlight_Raster(year, SP_Country)
    Raster_Pop <- Pop.Get_Population_Raster(year, SP_Country)
    
    print(paste0(LitPop.Class, ": Resampling Lit dimension with Pop"))
    Raster_Lit_Resampled <- raster::resample(Raster_Lit, Raster_Pop)
    Raster_LitPop <-
      (Raster_Lit_Resampled + ifelse(params[2] == 0, 0, 1)) ^ params[1] * Raster_Pop ^
      params[2]
    return(Raster_LitPop)
  }



LitPop.Get_LitPop_Proxy_Raster <-
  function(country_iso3,
           year = 2016,
           params = c(1, 1))
  {
    Raster_LitPop <-
      LitPop.Get_LitPop_Raster(country_iso3, year, params)
    DP_DEN_LitPop <- cellStats(Raster_LitPop, sum)
    Raster_LitPop_Proxy <- Raster_LitPop / DP_DEN_LitPop
    return(Raster_LitPop_Proxy)
  }


LitPop.Get_GDP_Proxy_Raster <-
  function(country_iso3,
           year = 2016,
           params = c(1, 1))
  {
    states_proxy <- MEF.Get_States_GDP_Proxy(country_iso3, year)
    cities_proxy <- MEF.GET_Cities_GDP_Proxy(country_iso3, year)
    
    Raster_GDP_Proxy <- LitPop.Get_GDP_Proxy_Raster_Base(country_iso3, year, params,states_proxy, cities_proxy)
    return(Raster_GDP_Proxy)
  }


LitPop.Get_GDP_Proxy_Raster_Base <-
  function(country_iso3,
           year = 2016,
           params = c(1, 1),
           states_proxy = NULL,
           cities_proxy = NULL)
  {
    Raster_GDP_Proxy <-
      LitPop.Get_LitPop_Proxy_Raster(country_iso3, year, params)
    
    if (is.null(states_proxy) & is.null(cities_proxy))
    {
      print(paste0(
        LitPop.Class,
        ": Returning base LitPop Proxy for: ",
        country_iso3
      ))
      return(Raster_GDP_Proxy)
    }
    
    SP_States <- getData(name = "GADM",
                         country = country_iso3,
                         level = 1,
                         path = Ref.DIR_DATA_Ref)
    SP_Cities <-
      getData(name = "GADM",
              country = country_iso3,
              level = 2, 
              path = Ref.DIR_DATA_Ref)
    
    #1. allocate proxy state by state
    for (id in SP_States$GID_1)
    {
      SP_State <- SP_States[SP_States$GID_1 == id,]
      print(paste0(
        LitPop.Class,
        ": Processing GDP proxy for state: ",
        SP_State$NAME_1
      ))
      
      Raster_State <- Raster_GDP_Proxy[SP_State, drop = F]
      Raster_GDP_Proxy[Raster_State] <- 0
      DP_State_GDP_Proxy <- cellStats(Raster_State, sum)
      
      if (!is.null(states_proxy) &
          nrow(states_proxy[states_proxy$GID_1 == id,]) != 0)
      {
        DP_State_GDP_Proxy <-
          states_proxy[states_proxy$GID_1 == id,]$GDP_Proxy
      }
      
      List_Raster_Cities <- c()
      if (!is.null(cities_proxy))
      {
        DF_Cities_GDP_Proxy <-
          cities_proxy[cities_proxy$GID_1 == id, , drop = F]
        
        if (nrow(DF_Cities_GDP_Proxy) != 0)
        {
          for (i in 1:nrow(DF_Cities_GDP_Proxy))
          {
            DF_City_GDP_Proxy <- DF_Cities_GDP_Proxy[i,]
            
            SP_City <-
              SP_Cities[SP_Cities$GID_2 == DF_City_GDP_Proxy$GID_2,]
            if (nrow(SP_City) != 1)
            {
              warning(paste0("Invalid GID_2: ", DF_City_GDP_Proxy$GID_2))
            }
            
            print(
              paste0(
                LitPop.Class,
                ": Processing city: ",
                SP_City$NAME_2,
                " with GDP proxy: ",
                DF_City_GDP_Proxy$GDP_Proxy
              )
            )
            
            Raster_City <- Raster_State[SP_City, drop = F]
            Raster_State[Raster_City] <- 0
            
            DP_DEN_City <- cellStats(Raster_City, sum)
            Raster_City <-
              Raster_City / DP_DEN_City * DF_City_GDP_Proxy$GDP_Proxy
            
            List_Raster_Cities <- c(List_Raster_Cities, Raster_City)
            DP_State_GDP_Proxy <-
              max(0,
                  (DP_State_GDP_Proxy - DF_City_GDP_Proxy$GDP_Proxy))
          }
        }
      }
      
      DP_DEN_State <- cellStats(Raster_State, sum)
      Raster_State <-
        Raster_State / DP_DEN_State * DP_State_GDP_Proxy
      if (length(List_Raster_Cities) != 0)
      {
        for (Raster_City in List_Raster_Cities) {
          Raster_State <- merge(Raster_City, Raster_State)
        }
      }
      print(
        paste0(
          LitPop.Class,
          ": Processed state: ",
          SP_State$NAME_1,
          " with GDP proxy: ",
          cellStats(Raster_State, sum)
        )
      )
      Raster_GDP_Proxy <- merge(Raster_State, Raster_GDP_Proxy)
    }
    DP_GDP_Proxy_Den <- cellStats(Raster_GDP_Proxy, sum)
    Raster_GDP_Proxy <- Raster_GDP_Proxy / DP_GDP_Proxy_Den
    return(Raster_GDP_Proxy)
  }
