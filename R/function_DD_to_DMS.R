#' DD_to_DMS
#'
#' @param DD_input latitude or longitude in decimal degrees (numeric)
#' @param axis one of `"horizontal"` (latitude) or `"vertical"` (longitude)
#'
#' @return latitude and/or longitude in degree minute second format
#'
#' @examples
#' DD_to_DMS(DD_input =  -63.9362, axis = "horizontal")
#' DD_to_DMS(DD_input = 44.5576, axis = "vertical")
#' 
#' @export
DD_to_DMS <-  function(DD_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  direction <- dplyr::case_when(
    axis == "horizontal" & DD_input < 0 ~ "W",
    axis == "horizontal" & !DD_input < 0 ~ "E",
    axis == "vertical" & DD_input < 0 ~ "S",
    axis == "vertical" & !DD_input < 0 ~ "N"
  )
  
  if(DD_input < 0) DD_input <-  DD_input * -1
  degree <- floor(DD_input)
  minute <- floor((DD_input - degree) * 60)
  second <- round(((DD_input - degree) * 60 - minute) * 60)
  
  DMS <- dplyr::case_when(
    axis == "vertical"  & !dplyr::between(degree, -90, 90) ~ NA_character_,
    axis == "horizontal" & !dplyr::between(degree, -180, 180) ~ NA_character_,
    TRUE ~ paste0(degree, intToUtf8(176), " ", minute, "' ", second, '"'," ", direction)
  )
  
  return(DMS)
}
