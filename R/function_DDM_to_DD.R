#' DDM_to_DD
#'
#' @param DDM_input latitude or longitude in degrees decimal minutes (character); e.g. `"63° 0 W"`
#' @param axis one of `"horizontal"` (latitude) or `"vertical"` (longitude)
#'
#' @return latitude and/or longitude in decimal degree format
#'
#' @examples
#' DDM_to_DD(DDM_input = "63° 56.172 W", axis = "horizontal")
#' DDM_to_DD(DDM_input = "44° 33.456 N", axis = "vertical")
#' 
#' @export
DDM_to_DD <-  function(DDM_input, axis){
  
  # Stop conditions
  if(!is.character(DDM_input)) stop("DDM_input must be a character string")
  if(length(DDM_input) != 1) stop("DDM_input must be of length 1")
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  # Detect whether character format uses NEWS to denote direction
  if(stringr::str_detect(DDM_input, "[NSEWnsew]")) {
    
    firstNum <- stringr::str_extract(DDM_input, "-?\\d+")
    minuteString <- stringr::str_replace(DDM_input, firstNum, "")
    secondNum <- stringr::str_extract(minuteString, "\\d+\\.*\\d*")
    leftover <- stringr::str_extract(minuteString, "[NSEWnsew]$")
    cleaned_DDM <- paste(firstNum, secondNum, leftover)
    DDM_parts <- stringr::str_split(cleaned_DDM, " ")[[1]]
    degrees <-  as.double(gsub("[^0-9.-]", "", DDM_parts[1]))
    minutes <- as.double(gsub("[^0-9.-]", "", DDM_parts[2]))
    direction <- DDM_parts[3]
    
    DD <- degrees + (minutes/60)
    
    if(direction == "S" | direction == "W"){ DD <- DD*-1}
    
  } else {
    
    firstNum <- stringr::str_extract(DDM_input, "-?\\d+")
    minuteString <- stringr::str_replace(DDM_input, firstNum, "")
    secondNum <- stringr::str_extract(minuteString, "\\d+\\.*\\d*")
    cleaned_DDM <- paste(firstNum, secondNum)
    DDM_parts <- stringr::str_split(cleaned_DDM, " ")[[1]]
    degrees <-  as.double(gsub("[^0-9.-]", "", DDM_parts[1]))
    minutes <- as.double(gsub("[^0-9.-]", "", DDM_parts[2]))
   
    DD <- abs(degrees) + minutes/60
    
    if(degrees < 0){DD <- DD*-1}
  }
  
  if ((direction %in% c("N", "S")) && abs(DD) > 90) {
    DD <- NaN
  } else if ((direction %in% c("W", "E")) && abs(DD) > 180) {
    DD <- NaN
  }
  

  return(DD)
  
}
