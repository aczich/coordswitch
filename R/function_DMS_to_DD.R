#' DMS_to_DD
#'
#' @param DMS_input latitude or longitude in degrees minute second format (character); e.g. `"63° 0' 0\" E"`
#' @param axis one of `"horizontal"` (latitude) or `"vertical"` (longitude)
#'
#' @return latitude and/or longitude in decimal degree format
#'
#' @examples
#' DMS_to_DD(DMS_input = "63° 56' 10\" W", axis = "horizontal")
#' DMS_to_DD(DMS_input = "44° 33' 27\" N", axis = "vertical")
#' 
#' @export
DMS_to_DD <-  function(DMS_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  firstNum <- stringr::str_extract(DMS_input, "-?\\d+")
  minuteString <- stringr::str_replace(DMS_input, firstNum, "")
  secondNum <- stringr::str_extract(minuteString, "\\d+")
  secondString <- stringr::str_replace(minuteString, secondNum, "")
  thirdNum <- stringr::str_extract(secondString, "\\d+\\.*\\d*")
  leftover <- stringr::str_extract(secondString, "[NSEWnsew]$")
  cleaned_DMS <- paste(firstNum, secondNum, thirdNum, leftover)
  DMS_parts <- stringr::str_split(cleaned_DMS, " ")[[1]]
  
  degrees <-  as.double(gsub("[^0-9.-]", "", DMS_parts[1]))
  minutes <- as.double(gsub("[^0-9.-]", "", DMS_parts[2]))
  seconds <- as.double(gsub("[^0-9.-]", "", DMS_parts[3]))
  
  direction <- DMS_parts[4]
  DD <- degrees + (minutes/60) + (seconds/3600) 

  DD <- dplyr::case_when(
    direction %in% c("S", "W") ~ DD * -1,
    axis == "vertical" & DD > 90 ~ NaN,
    axis == "horizontal" & DD > 180 ~ NaN,
    TRUE ~ DD
  )
  
  return(DD)

}
