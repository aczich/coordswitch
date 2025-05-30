#' DD_to_DDM
#'
#' @param DD_input latitude or longitude in decimal degrees (numeric)
#' @param axis one of `"horizontal"` (latitude) or `"vertical"` (longitude)
#'
#' @return latitude or longitude in decimal degree minute format
#'
#' @examples
#' DD_to_DDM(DD_input = -63.9362, axis = "horizontal")
#' DD_to_DDM(DD_input = 44.5576, axis = "vertical")
#' 
#' @export
DD_to_DDM <-  function(DD_input, axis){
  
  # Stop conditions
  if(!is.numeric(DD_input)) stop("DD_input must be numeric")
  if(length(DD_input) != 1) stop("DD_input must be of length 1")
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  absDeg <- abs(DD_input)
  degree <- trunc(absDeg)
  minute <- (absDeg - degree) * 60
  
  direction <- dplyr::case_when(
    axis == "horizontal" & DD_input < 0 ~ "W",
    axis == "horizontal" & !DD_input < 0 ~ "E",
    axis == "vertical" & DD_input < 0 ~ "S",
    axis == "vertical" & !DD_input < 0 ~ "N"
  )
 
  DDM <- dplyr::case_when(
    axis == "vertical"  & !dplyr::between(DD_input, -90, 90) ~ NA_character_,
    axis == "horizontal" & !dplyr::between(DD_input, -180, 180) ~ NA_character_,
    TRUE ~ paste0(degree,intToUtf8(176), " ", minute, " ", direction)
  )
  
  return(DDM)
}
