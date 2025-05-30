require(sp)
require(tidyverse)
require(glue)
require(stringr)

source("CoordinateConversion/R/function_DD_to_DDM.R")
source("CoordinateConversion/R/function_DD_to_DMS.R")
source("CoordinateConversion/R/function_DD_to_UTM.R")
source("CoordinateConversion/R/function_DDM_to_DD.R")
source("CoordinateConversion/R/function_DDM_to_DMS.R")
source("CoordinateConversion/R/function_DDM_to_UTM.R")
source("CoordinateConversion/R/function_DMS_to_DD.R")
source("CoordinateConversion/R/function_DMS_to_DDM.R")
source("CoordinateConversion/R/function_DMS_to_UTM.R")
source("CoordinateConversion/R/function_UTM_to_DD.R")
source("CoordinateConversion/R/function_UTM_to_DMS.R")
source("CoordinateConversion/R/function_UTM_to_DDM.R")
source("CoordinateConversion/R/sp_convert.R")

# TO DO ----
# Add supressWarning()
# Add better error messages if x and y classes are incorrect

# Testing ----
# From DD
testdata_DD <- data.frame(lon = c( -63, -89.2345, 4.5, 4.5),
                          lat = c(44, 23.34, 57.3246, 57.3246),
                          record = c("A", "B", "C", "D"))

sp_convert(testdata_DD) # Works
sp_convert(testdata_DD, to = "DMS") # Works
sp_convert(testdata_DD, to = "DDM") # Works
sp_convert(testdata_DD, to = "UTM") # Works
sp_convert(testdata_DD, to = c("DDM", "DMS")) # Works
sp_convert(testdata_DD, to = c("DDM", "DMS", "DD")) # Works

sp_convert(testdata_DD, from = "DD", to = "DMS")
sp_convert(testdata_DD, from = "DD", to = "all")

# From DDM
testdata_DDM <- sp_convert(testdata_DD, to = "DDM") %>%
  select(lat = lat_DDM, lon = lon_DDM)

sp_convert(testdata_DDM, from = "DDM") # Works
sp_convert(testdata_DDM, from = "DDM", to = "DD") # Works
sp_convert(testdata_DDM, from = "DDM", to = "DMS") # Works
sp_convert(testdata_DDM, from = "DDM", to = "UTM") # Works
sp_convert(testdata_DDM, from = "DDM", to = c("DMS", "DD")) # Works
sp_convert(testdata_DDM, from = "DDM", to = c("DMS", "DD", "DDM")) # Works

# From DMS
testdata_DMS <- sp_convert(testdata_DD, to = "DMS") %>%
  select(lat = lat_DMS, lon = lon_DMS)

sp_convert(testdata_DMS, from = "DMS") # Works
sp_convert(testdata_DMS, from = "DMS", to = "DD") # Works
sp_convert(testdata_DMS, from = "DMS", to = "DDM") # Works
sp_convert(testdata_DMS, from = "DMS", to = "UTM") # Works

# From UTM
testdata_UTM <- sp_convert(testdata_DD, to = "UTM") %>%
  select(easting, northing, zone)

sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = "DD") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = "DDM") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = "DMS") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = c("DMS", "DD", "UTM")) # Works

UTM_to_DD(x = 271542.3, y = 2582924, zone = 16, return = "all")
UTM_to_DD(x = 271542.3, y = 2582924, zone = 16, return = "lat")
UTM_to_DD(x = 271542.3, y = 2582924, zone = 16, return = "lon")

UTM_to_DDM(x = 271542.3, y = 2582924, zone = 16, return = "all")
UTM_to_DDM(x = 271542.3, y = 2582924, zone = 16, return = "lat")
UTM_to_DDM(x = 271542.3, y = 2582924, zone = 16, return = "lon")

UTM_to_DMS(x = 271542.3, y = 2582924, zone = 16, return = "all")
UTM_to_DMS(x = 271542.3, y = 2582924, zone = 16, return = "lat")
UTM_to_DMS(x = 271542.3, y = 2582924, zone = 16, return = "lon")

DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "all")
DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "easting")
DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "northing")
DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "zone")

DD_to_UTM(x = -63, y = 44, return = "all")
DD_to_UTM(x = -63, y = 44, return = "easting")
DD_to_UTM(x = -63, y = 44, return = "northing")
DD_to_UTM(x = -63, y = 44, return = "zone")

# Parameters
# data = dataframe with spatial data
# x - "longitude column name"
# y - "latitude column name"
# from - what format is your spatial data in currently 
# DD -> Decimal Degrees
# DMS -> Degrees, minutes, seconds
# DDM <- Degrees, decimal minutes
# UTM -> UTM 
#If from = UTM, then must include UTM = "zone column name"
#to - default is "all", meaning that function will convert to all fromats automatically
#however, DD, DCM, DDM, or UTM can be entered to convert it to one of those 
#can specify which ellipsoid to use, default is WGS78
# eFish tests ----
eFish <- read.csv("eFish.csv") %>%
  mutate(easting = as.numeric(easting),
         northing = as.numeric(northing),
         zone = 20)

sp_convert(eFish %>% slice(1:100), x = "easting", y = "northing",
           zone = "zone", from = "UTM", to = "DDM")
