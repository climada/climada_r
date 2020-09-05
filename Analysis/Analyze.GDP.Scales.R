library(readxl)
IMF_GDP_Quarterly_GDP <-
  read_excel("Data/MEF/IMF-GDP-Quarterly GDP.xlsx")
IMF_GDP_Quarterly_GDP <-
  IMF_GDP_Quarterly_GDP[IMF_GDP_Quarterly_GDP$MEASURE == "PC_CHGPY",]

IMF_GDP_Quarterly_GDP$Change <- 0
DF_Data_L0 <- data.frame(
  LOCATION = character(),
  INDICATOR = character(),
  SUBJECT = character(),
  MEASURE = character(),
  FREQUENCY = character(),
  TIME = character(),
  Value = numeric(),
  "Flag Codes" = character(),
  Change = numeric()
)

for (iso3 in unique(IMF_GDP_Quarterly_GDP$LOCATION)) {
  DF_Data <-
    IMF_GDP_Quarterly_GDP[IMF_GDP_Quarterly_GDP$LOCATION == iso3, ]
  DP_Rows <- nrow(DF_Data)
  DF_Data[c(2:DP_Rows),]$Change <-
    DF_Data[c(2:DP_Rows),]$Value - DF_Data[c(1:(DP_Rows - 1)), ]$Value
  DF_Data_L0 <- rbind(DF_Data_L0, DF_Data)
}

DF_Data_L0$Year <- as.numeric(substring(DF_Data_L0$TIME, 1, 4))
DF_Data_L0 <-
  DF_Data_L0[DF_Data_L0$Year >= 2000 & DF_Data_L0$Year < 2020, ]

DF_GDP_Grid <- data.frame(LOCATION = character(),
                          Change = numeric(),
                          Type = character())
for (year in c(1, 3, 5, 10, 20)) {
  DF_Data_L0$Group <- floor(DF_Data_L0$Year / year)
  DF_Data_L1 <- aggregate(
    DF_Data_L0[, c("Change")],
    by = list(LOCATION = DF_Data_L0$LOCATION,
              Group = DF_Data_L0$Group),
    FUN = min
  )
  
  DF_Data_L2 <- aggregate(DF_Data_L1[, c("Change")],
                          by = list(LOCATION = DF_Data_L1$LOCATION),
                          FUN = mean)
  DF_Data_L2$Type <- paste0("1 in ", year)
  DF_GDP_Grid <- rbind(DF_GDP_Grid, DF_Data_L2)
}
colnames(DF_GDP_Grid) <- c("Location", "Change", "Type")
DF_GDP_Grid$Change <- round(DF_GDP_Grid$Change, digits = 1)
write.csv(DF_GDP_Grid, "Quarterly YoY GDP Change Grid.csv")
