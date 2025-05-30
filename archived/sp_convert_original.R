# ORIGINAL
sp_convert <- function(data = data, x = "longitude", y = "latitude", UTM = "zone", from, to = "all", keep = TRUE, ellipsoid = "WGS84"){
  
  require(sp)
  require(tidyverse)
  naData <- data %>% rename(x = paste(x), y = paste(y)) %>% filter(is.na(x)| is.na(y))
  
  #rename x and y for the function to run long_DDM etc..
  data <- data %>% rename(x = paste(x), y = paste(y)) %>% filter(!is.na(x), !is.na(y))
  
  if("UTM" %in% names(data)){
    data <- data %>% rename(zone = paste(UTM))
    distinctData <- data %>% distinct(x,y,zone)
  }
  
  distinctData <- data %>% distinct(x,y)
  #input data into the function, then apply rowwise() to the data and take the specified column names and perform calculations
  #conv_coords <- function(longitude_entry , latitude_entry, UTM, from, to = "all", ellipsoid = "WGS84"){
  
  ##Conversion Functions ----
  ## DD to DMS function
  DD_to_DMS <-  function(decimalDegree, axis){
    
    if(axis == "horizontal"){
      suffix <-  ifelse(decimalDegree < 0, "W", "E")
    }
    if(axis == "vertical"){
      suffix <-  ifelse(decimalDegree < 0, "S", "N")
    }
    
    if(decimalDegree < 0){
      decimalDegree <-  decimalDegree*-1
    }
    
    degree <-  floor(decimalDegree)
    minute <-  floor((decimalDegree-degree)*60)
    second <-   round(((decimalDegree-degree)*60 - minute)*60)
    
    
    DMS <- paste0(degree,intToUtf8(176), " ", minute, "' ", second, '"'," ", suffix)
    if(((degree < -90| degree > 90) & axis == "vertical") | ((degree < -180| degree > 180) & axis == "horizontal") ){
      DMS <- "Error: Invalid Coordinate"
    }
    return(DMS)
  }
  
  #DD to DDM ----
  DD_to_DDM <-  function(decimalDegree, axis){
    absDeg <- abs(decimalDegree)
    
    degree <-  trunc(absDeg)
    minute <-  (absDeg-degree)*60
    
    direction <- case_when(decimalDegree >= 0 & axis == "vertical"~ "N",
                           decimalDegree < 0 & axis == "vertical"~ "S",
                           decimalDegree >= 0 & axis == "horizontal"~ "E",
                           decimalDegree < 0 & axis == "horizontal"~ "W")
    
    
    DDM <- paste0(degree,intToUtf8(176), " ", minute, " ",direction)
    if(((decimalDegree < -90| degree > 90) & axis == "vertical") | ((degree < -180| degree > 180) & axis == "horizontal") ){
      DDM <- "Error: Invalid Coordinate"
    }
    return(DDM)
  }
  
  
  
  
  #DMS to DD Function    
  DMS_to_DD <-  function(DMS_input, axis){
    
    firstNum <- str_extract(DMS_input, "-?\\d+")
    minuteString <- str_replace(DMS_input, firstNum, "")
    secondNum <- str_extract(minuteString, "\\d+")
    secondString <- str_replace(minuteString, secondNum, "")
    thirdNum <- str_extract(secondString, "\\d+\\.*\\d*")
    leftover <- str_extract(secondString, "[NSEWnsew]$")
    cleaned_DMS <- paste(firstNum, secondNum, thirdNum, leftover)
    
    
    DMS_parts <- str_split(cleaned_DMS, " ")[[1]]
    
    degrees <-  as.double(gsub("[^0-9.-]", "", DMS_parts[1]))
    minutes <- as.double(gsub("[^0-9.-]", "", DMS_parts[2]))
    seconds <- as.double(gsub("[^0-9.-]", "", DMS_parts[3]))
    direction <- DMS_parts[4]
    
    DD = degrees + (minutes/60) + (seconds/3600)
    if(direction == "S" | direction == "W"){
      DD <- DD*-1
    }
    if((axis == "vertical" & DD > 90) | (axis == "horizontal" & DD > 180)){
      Print("Error: Invalid Coordinate")
    }
    
    return(DD)
  }
  
  DDM_to_DD <- function(DDM_input, axis){
    if(str_detect(DDM_input, "[NSEWnsew]")){ #detect whether format uses - or NESW to denote direction
      
      firstNum <- str_extract(DDM_input, "-?\\d+")
      minuteString <- str_replace(DDM_input, firstNum, "")
      secondNum <- str_extract(minuteString, "\\d+\\.*\\d*")
      leftover <- str_extract(minuteString, "[NSEWnsew]$")
      cleaned_DDM <- paste(firstNum, secondNum, leftover)
      
      DDM_parts <- str_split(cleaned_DDM, " ")[[1]]
      degrees <-  as.double(gsub("[^0-9.-]", "", DDM_parts[1]))
      minutes <- as.double(gsub("[^0-9.-]", "", DDM_parts[2]))
      direction <- DDM_parts[3]
      
      
      DD = degrees + (minutes/60)
      if(direction == "S" | direction == "W"){
        DD <- DD*-1
      }
      
      
    } else{
      firstNum <- str_extract(DDM_input, "-?\\d+")
      minuteString <- str_replace(DDM_input, firstNum, "")
      secondNum <- str_extract(minuteString, "\\d+\\.*\\d*")
      cleaned_DDM <- paste(firstNum, secondNum)
      
      DDM_parts <- str_split(cleaned_DDM, " ")[[1]]
      
      degrees <-  as.double(gsub("[^0-9.-]", "", DDM_parts[1]))
      minutes <- as.double(gsub("[^0-9.-]", "", DDM_parts[2]))
      
      
      
      DD <- abs(degrees) + 
        minutes/60
      
      if(degrees < 0){
        DD <- DD*-1
      }
    }
    
    if((axis == "vertical" & DD > 90) | (axis == "horizontal" & DD > 180)){
      Print("Error: Invalid Coordinate")
    }
    return(DD)
  }
  
  #DD to UTM
  get_UTMZone <- function(lat,lon) { #change to ifelse
    ## Special Cases for Norway & Svalbard
    zone <- case_when(lat >= 56 & lat < 64 & lon >= 3 & lon < 6 ~ 32,
                      lat >= 72 & lon >= 6 & lon < 9 ~ 31,
                      lat >= 72 & lon >= 9 & lon < 12 ~ 33,
                      lat >= 72 & lon >= 18 & lon < 21 ~ 33,
                      lat >= 72 & lon >= 21 & lon < 24 ~ 35,
                      lat >= 72 & lon >= 30 & lon < 33 ~ 35,
                      lat >= 72 & lon >= 33 & lon <42 ~ 37,
                      lon >= -180 & lon <= 180 ~ (floor((lon + 180)/6) %% 60) + 1,
                      TRUE ~ NA_real_)
    
    return(zone)
  }
  
  
  DD_to_UTM <- function(lat, lon){
    
    zone <- get_UTMZone(lat, lon)
    
    xy <- data.frame(easting = lon, northing = lat, zone = zone)
    coordinates(xy) <- c("easting", "northing")
    proj4string(xy) <- CRS("+proj=longlat + datum=WGS84") #change according to ellipsoid
    
    res <- spTransform(xy, CRS(paste0("+proj=utm +zone=", zone, "ellps=WGS84")))
    en <- as.data.frame(res) %>% select(easting, northing, zone)
    return(en)
  }
  
  
  UTM_to_DD <- function(easting, northing, zone){
    utm <- data.frame(lon = easting, lat = northing)
    coordinates(utm) <- ~lon+lat
    class(utm)
    proj4string(utm) <- CRS(paste0("+proj=utm +zone=", zone, "+ellps=WGS84"))
    utm <- spTransform(utm, CRS("+proj=longlat +datum=WGS84"))
    output <- as.data.frame(utm) %>% select(latitude_DD = lat,longitude_DD = lon)
    
    return(output)
  }
  
  #Applying Conversion Functions ----
  #data %>% rowwise() %>% mutate(DD_to_DMS(longitude_entry, axis = "horizontal"))
  
  #Covert to DD first
  if(from != "DD"){
    if(from == "DMS"){
      step1 <-  distinctData %>% rowwise() %>% mutate(latitude_DD = DMS_to_DD(y, axis = "vertical"),
                                                      longitude_DD = DMS_to_DD(x, axis = "horizontal"))
    }
    if(from == "DDM"){
      step1 <- distinctData %>% rowwise() %>% mutate(latitude_DD = DDM_to_DD(y, axis = "vertical"),
                                                     longitude_DD = DDM_to_DD(x, axis = "horizontal"))
    }
    if(from == "UTM"){
      step1 <- distinctData %>% rowwise() %>% mutate(UTM_to_DD(x, y, zone))
    }
  } else{
    step1 <- distinctData %>% mutate(latitude_DD = y, longitude_DD = x)
  }
  #DMS to UTM
  #From Decimal Degrees to DMS
  if((to == "all"|to == "DMS")){
    step1 <-  step1 %>% rowwise() %>% mutate(latitude_DMS = DD_to_DMS(latitude_DD, axis = "vertical"),
                                             longitude_DMS = DD_to_DMS(longitude_DD, axis = "horizontal"))
  }
  #From  Decimal degrees to DDM
  if(to == "all"|to == "DDM"){
    step1 <-  step1 %>% rowwise() %>% mutate(latitude_DDM = DD_to_DDM(latitude_DD, axis = "vertical"),
                                             longitude_DDM = DD_to_DDM(longitude_DD, axis = "horizontal"))
  }
  #From decimal degrees to UTM
  if((to == "all"|to == "UTM")){
    step1 <-  step1 %>% rowwise() %>% mutate(DD_to_UTM(latitude_DD,longitude_DD))
  }
  
  
  if(keep == TRUE){
    #   if(from == "DMS"){
    #     step1 <- step1 %>% rename(longitude_DMS = x, latitude_DMS = y)
    #   }else if(from == "DDM"){
    #     step1 <- step1 %>% rename(longitude_DDM = x, latitude_DDM = y)
    #   }else if(from == "UTM"){
    #     step1 <- step1 %>% rename(easting = x, northing = y)
    #   }
    #   if(from != "DD" & to != "DD" & to != "all"){
    #     print("yes")
    #     step1 <- step1 %>% select(-latitude_DD, -longitude_DD)
    #   }
    # } else {
    #step1 <- step1 %>% select(-x, -y)
    if(from == "UTM"){
      step1 <- step1 %>% select(-zone)
    }
  }
  
  if(length(data %>% select(-c(x,y))) > 0){
    outputData <- full_join(left_join(data, step1, by = c("y" = "y",  "x" = "x")), naData) %>% select(-c(x,y))
  } else{
    outputData <- step1
  }
  
  return(outputData)
}





#Parameters
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
#keep = TRUE as defauly, which means it will keep your original format, otherwise,
#as FALSE it will remove your original format and only keep the new ones
#can specify which ellipsoid to use, default is WGS78


#Example with some test data
testdata <- data.frame(lat = c(44, 23.34, 57.3246 ), lon = c( -63, -89.2345 ,4.5), otherData = c("A", "B", "C")) #20,16,32

mydata <- sp_convert(testdata, x = "lon", y = "lat", from = "DD");mydata