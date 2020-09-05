require(readxl)
require(reshape2)
require(wbstats)

source("Ref.R")

MEF.Class <- 'MEF'



MEF.Get_Produced_Capital <- function(country_iso3, year)
{
  DF_Produced_Capital <-
    MEF.Get_MEF_Value_Fr_WB(country_iso3, NULL, "WB", "Produced Capital", "NW.PCA.TO")
  FN_Produced_Capital <-
    approxfun(DF_Produced_Capital$date, DF_Produced_Capital$value)
  DP_Produced_Capital <- FN_Produced_Capital(year)
  
  if (is.na(DP_Produced_Capital))
  {
    DP_Year_Min <- min(DF_Produced_Capital$date)
    DP_Year_Max <- max(DF_Produced_Capital$date)
    DP_Year <- ifelse(year < DP_Year_Min, DP_Year_Min, DP_Year_Max)
    DP_Produced_Capital <-
      MEF.Get_GDP_Value(country_iso3, year)$value / MEF.Get_GDP_Value(country_iso3, DP_Year)$value * DF_Produced_Capital[DF_Produced_Capital$date == DP_Year, c("value")]
  }
  
  print(paste0(
    MEF.Class,
    ": Returning ",
    country_iso3,
    " Produced Capital Info for ",
    year
  ))
  return(DP_Produced_Capital / 1.24)
}



MEF.Get_GDP_Value <- function(country_iso3, year)
{
  DF_GDP_Value <-
    MEF.Get_MEF_Value_Fr_WB(country_iso3, year, "WB", "GDP", "NY.GDP.MKTP.CD")
  print(paste0(MEF.Class, ": Returning ", country_iso3, " GDP Info for ", year))
  return(DF_GDP_Value)
}



MEF.Get_States_GDP_Proxy <- function(country_iso3, year)
{
  #1. get the state proxy if available
  DP_GDP_States_Path <-
    paste0(Ref.DIR_DATA_MEF, country_iso3, " States GDP Proxy.xlsx")
  if (file.exists(DP_GDP_States_Path))
  {
    DF_GDP_States_FrFile <-
      read_excel(DP_GDP_States_Path, sheet = "Data")
    DF_GDP_States_Melted <-
      melt(
        DF_GDP_States_FrFile,
        id = c(1:4),
        na.rm = T,
        variable.name = "Year",
        value.name = "GDP"
      )
    DF_GDP_States_Melted$Year <-
      as.numeric(levels(DF_GDP_States_Melted$Year))[DF_GDP_States_Melted$Year]
    VT_Years <- unique(DF_GDP_States_Melted$Year)
    DP_Year <- MEF.Get_Closest_Year(VT_Years, year)
    
    if (DP_Year == year) {
      print(
        paste0(
          MEF.Class,
          ": Returning ",
          country_iso3,
          " states GDP with ",
          DP_Year,
          " value"
        )
      )
    } else{
      print(
        paste0(
          MEF.Class,
          ": Returning ",
          country_iso3,
          " states GDP with ",
          DP_Year,
          " proxy"
        )
      )
    }
    
    DF_GDP_States_Melted <-
      DF_GDP_States_Melted[DF_GDP_States_Melted$Year == DP_Year,]
    DF_GDP_States_Melted$GDP_Proxy <-
      DF_GDP_States_Melted$GDP / sum(DF_GDP_States_Melted$GDP)
    return(DF_GDP_States_Melted)
  } else {
    print(paste0(MEF.Class,
                 ": ",
                 DP_GDP_States_Path,
                 " file is not found."))
    return(NULL)
  }
}



MEF.GET_Cities_GDP_Proxy <- function(country_iso3, year)
{
  DP_GDP_Cities_Path <-
    paste0(Ref.DIR_DATA_MEF, country_iso3, " Cities GDP.xlsx")
  if (file.exists(DP_GDP_Cities_Path))
  {
    DF_GDP_Cities_FrFile <-
      read_excel(DP_GDP_Cities_Path, sheet = "Data")
    DF_GDP_Cities_Melted <-
      melt(
        DF_GDP_Cities_FrFile,
        id = c(1:5),
        na.rm = T,
        variable.name = "Year",
        value.name = "GDP"
      )
    DF_GDP_Cities_Melted$Year <-
      as.numeric(levels(DF_GDP_Cities_Melted$Year))[DF_GDP_Cities_Melted$Year]
    VT_Years <- unique(DF_GDP_Cities_Melted$Year)
    DP_Year <- MEF.Get_Closest_Year(VT_Years, year)
    
    DP_DEN_GDP <-
      MEF.Get_GDP_Value(country_iso3, DP_Year)$value / 10 ^ 6
    
    if (DP_Year == year) {
      print(
        paste0(
          MEF.Class,
          ": Returning ",
          country_iso3,
          " cities GDP with ",
          DP_Year,
          " value"
        )
      )
    } else{
      print(
        paste0(
          MEF.Class,
          ": Returning ",
          country_iso3,
          " cities GDP with ",
          DP_Year,
          " proxy"
        )
      )
    }
    
    DF_GDP_Cities_Melted <-
      DF_GDP_Cities_Melted[DF_GDP_Cities_Melted$Year == DP_Year,]
    DF_GDP_Cities_Melted$GDP_Proxy <-
      DF_GDP_Cities_Melted$GDP / DP_DEN_GDP
    return(DF_GDP_Cities_Melted)
  } else {
    print(paste0(MEF.Class,
                 ": ",
                 DP_GDP_Cities_Path,
                 " file is not found."))
    return(NULL)
  }
}



MEF.Get_Closest_Year <- function(years, year)
{
  DF_Years <- data.frame(Year = years, Diff = abs(years - year))
  DF_Years <- DF_Years[order(DF_Years$Diff, DF_Years$Year),]
  DP_Year <- DF_Years[1, c(1)]
  return(DP_Year)
}



MEF.Get_MEF_Value_Fr_WB <-
  function(country_iso3, year, source, name, id)
  {
    PT_Data_Path <-
      paste0(Ref.DIR_DATA_MEF, source, "-", name, "-", id, ".csv")
    
    if (file.exists(PT_Data_Path))
    {
      DF_Data_Countries <- read.csv(PT_Data_Path, stringsAsFactors = F)
      DF_Data_Countries <- DF_Data_Countries[,-c(1)]
      DF_Data_Year <- DF_Data_Country <-
        DF_Data_Countries[DF_Data_Countries$iso3c == country_iso3, ]
    } else
    {
      DF_Data_Year <- DF_Data_Country <-
        DF_Data_Countries <- data.frame(
          iso3c = character(),
          date = integer(),
          value = numeric(),
          indicatorID = character(),
          indicator = character(),
          iso2c = character(),
          country = character()
        )
    }
    
    if (!is.null(year)) {
      DF_Data_Year <-
        DF_Data_Country[DF_Data_Country$date == year, ]
    }
    
    if (nrow(DF_Data_Year) == 0) {
      print(paste0(
        MEF.Class,
        ": Extracting ",
        country_iso3,
        " info from World Bank website"
      ))
      try({
        DF_Data_Country <- wb(country = country_iso3, id)
        if (nrow(DF_Data_Country) != 0) {
          DF_Data_Country$date <- as.integer(DF_Data_Country$date)
          DF_Data_Countries <-
            rbind(DF_Data_Countries[DF_Data_Countries$iso3c != country_iso3,], DF_Data_Country)
          write.csv(DF_Data_Countries[order(iso3c,-date)], file = PT_Data_Path)
        }
      }, silent = T)
    }
    
    if (is.null(year)) {
      return(DF_Data_Country)
    }
    
    DP_Year <- MEF.Get_Closest_Year(DF_Data_Country$date, year)
    if (DP_Year != year)
    {
      warning(
        paste(
          country_iso3,
          name,
          "data",
          id,
          "for",
          year,
          "is not available, proxy to",
          DP_Year
        )
      )
    }
    
    DF_Data_Year <-
      DF_Data_Country[DF_Data_Country$date == DP_Year, ]
    return(DF_Data_Year)
  }



MEF.Get_Country_Info_Fr_WB <- function(country = NULL)
{
  PT_Countries_Path <- paste0(Ref.DIR_DATA_MEF, "WB-Countries.csv")
  if (file.exists(PT_Countries_Path)) {
    DF_Countries <- read.csv(PT_Countries_Path, stringsAsFactors = F)
    DF_Countries <- DF_Countries[,-c(1)]
  } else{
    DF_Countries <- wbcountries(lang = c("en"))
    write.csv(DF_Countries, file = PT_Countries_Path)
  }
  if (is.null(country))
  {
    print(paste0(MEF.Class, ": Extracting country info list"))
    return(DF_Countries)
  } else
  {
    print(paste0(MEF.Class, ": Extracting country info for ", country))
    return (DF_Countries[DF_Countries$country == country, , drop = F])
  }
}
