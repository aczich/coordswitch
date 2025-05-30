#' DDM_to_DMS
#'
#' @param DDM_input latitude or longitude in degrees decimal minutes (character); e.g. `"63° 0 W"`
#' @param axis one of `"horizontal"` (latitude) or `"vertical"` (longitude)
#'
#' @return latitude and/or longitude in degree minute second format
#'
#' @examples
#' DDM_to_DMS(DDM_input = "63° 0 W", axis = "horizontal")
#' DDM_to_DMS(DDM_input = "44° 0 N", axis = "vertical")
#' 
#' @export
DDM_to_DMS <-  function(DDM_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  DD_to_DMS(DD_input = DDM_to_DD(DDM_input, axis), axis)
  
}
