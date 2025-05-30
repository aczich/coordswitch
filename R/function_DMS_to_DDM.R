#' DMS_to_DDM
#'
#' @param DMS_input latitude or longitude in degrees minute second format (character); e.g. `"63° 0' 0\" E"`
#' @param axis one of `"horizontal"` (latitude) or `"vertical"` (longitude)
#'
#' @return latitude and/or longitude in decimal degree minute format
#'
#' @examples
#' DMS_to_DDM(DMS_input = "63° 0' 0\" E", axis = "horizontal")
#' DMS_to_DDM(DMS_input = "44° 0' 0\" N", axis = "vertical")
#' 
#' @export
DMS_to_DDM <-  function(DMS_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  DD_to_DDM(DD_input = DMS_to_DD(DMS_input, axis), axis)
  
}
