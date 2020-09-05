Folder for states meta data from below function:

source("Ref.R")
SP_States <- getData(name = "GADM",
                     country = DP_Country_ISO3,
                     level = 1,
		     path = Ref.DIR_DATA_Ref)
write.csv(SP_States@data, file = paste0(Ref.DIR_DATA_Ref, DP_Country_ISO3, " States.csv"))
