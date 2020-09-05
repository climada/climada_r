# Generate Comparison Files for GDP Allocation
DP_Year <- 2018
DP_Country_ISO3 <- "AUS"
source("Ref.R")

#1. States GDP Comparison
source(Ref.DIR_FUNC_MEF)
DF_GDP_States_Proxy <-
  MEF.Get_States_GDP_Proxy(DP_Country_ISO3, DP_Year)

source(Ref.DIR_FUNC_LitPop)
Raster_GDP_Proxy_BaseLn <-
  LitPop.Get_GDP_Proxy_Raster_Base(DP_Country_ISO3, DP_Year)
Raster_GDP_Proxy_States <-
  LitPop.Get_GDP_Proxy_Raster_Base(DP_Country_ISO3, DP_Year,
                                   states_proxy = DF_GDP_States_Proxy)

SP_States <- getData(
  name = "GADM",
  country = DP_Country_ISO3,
  path = Ref.DIR_DATA_Ref,
  level = 1
)

DF_States_Compare <- data.frame(
  State_Name = character(),
  Expect_GDP = numeric(),
  BaseLn_GDP = numeric(),
  States_GDP = numeric(),
  Cities_GDP = numeric()
)
for (i in 1:nrow(DF_GDP_States_Proxy))
{
  DF_GDP_State_Proxy <- DF_GDP_States_Proxy[i,]
  SP_State <-
    SP_States[SP_States$GID_1 == DF_GDP_State_Proxy$GID_1, ]
  
  Raster_State_BaseLn <- Raster_GDP_Proxy_BaseLn[SP_State, drop = F]
  DP_GDP_State_BaseLn <- cellStats(Raster_State_BaseLn, sum)
  
  Raster_State_States <- Raster_GDP_Proxy_States[SP_State, drop = F]
  DP_GDP_State_States <- cellStats(Raster_State_States, sum)
  
  Raster_State_Cities <- Raster_GDP_Proxy_Cities[SP_State, drop = F]
  DP_GDP_State_Cities <- cellStats(Raster_State_Cities, sum)
  
  DF_State_Record <-
    data.frame(
      State_Name = c(DF_GDP_State_Proxy$State_Name_EN),
      Expect_GDP = c(DF_GDP_State_Proxy$GDP_Proxy),
      BaseLn_GDP = c(DP_GDP_State_BaseLn),
      States_GDP = c(DP_GDP_State_States),
      Cities_GDP = c(DP_GDP_State_Cities)
    )
  DF_States_Compare <- rbind(DF_States_Compare, DF_State_Record)
}
write.csv(DF_States_Compare,
          paste0(DP_Country_ISO3, " States GDP Comparison.csv"))



#2. Cities GDP Comparison
DF_GDP_Cities_Proxy <-
  MEF.GET_Cities_GDP_Proxy(DP_Country_ISO3, DP_Year)

if (!is.null(DF_GDP_Cities_Proxy)) {
  Raster_GDP_Proxy_Cities <-
    LitPop.Get_GDP_Proxy_Raster_Base(DP_Country_ISO3,
                                     DP_Year,
                                     states_proxy = DF_GDP_States_Proxy,
                                     cities_proxy = DF_GDP_Cities_Proxy)
  
  SP_Cities <- getData(
    name = "GADM",
    country = DP_Country_ISO3,
    path = Ref.DIR_DATA_Ref,
    level = 2
  )
  
  DF_Cities_Compare <- data.frame(
    City_Name = character(),
    Expect_GDP = numeric(),
    BaseLn_GDP = numeric(),
    States_GDP = numeric(),
    Cities_GDP = numeric()
  )
  for (i in 1:nrow(DF_GDP_Cities_Proxy))
  {
    DF_GDP_City_Proxy <- DF_GDP_Cities_Proxy[i,]
    SP_City <-
      SP_Cities[SP_Cities$GID_2 == DF_GDP_City_Proxy$GID_2, ]
    
    Raster_City_BaseLn <- Raster_GDP_Proxy_BaseLn[SP_City, drop = F]
    DP_GDP_City_BaseLn <- cellStats(Raster_City_BaseLn, sum)
    
    Raster_City_States <- Raster_GDP_Proxy_States[SP_City, drop = F]
    DP_GDP_City_States <- cellStats(Raster_City_States, sum)
    
    Raster_City_Cities <- Raster_GDP_Proxy_Cities[SP_City, drop = F]
    DP_GDP_City_Cities <- cellStats(Raster_City_Cities, sum)
    
    DF_City_Record <-
      data.frame(
        City_Name = c(DF_GDP_City_Proxy$City_Name),
        Expect_GDP = c(DF_GDP_City_Proxy$GDP_Proxy),
        BaseLn_GDP = c(DP_GDP_City_BaseLn),
        States_GDP = c(DP_GDP_City_States),
        Cities_GDP = c(DP_GDP_City_Cities)
      )
    DF_Cities_Compare <- rbind(DF_Cities_Compare, DF_City_Record)
  }
  write.csv(DF_States_Compare,
            paste0(DP_Country_ISO3, " Cities GDP Comparison.csv"))
}
