#' DD_to_UTM
#'
#' @param x latitude in decimal degrees (numeric)
#' @param y longitude in decimal degrees (numeric)
#' @param ellipsoid reference ellipsoid (character); defaults to `"WGS40"`
#' @param return desired output; one of `"all"`, `"northing"`, `"easting"`, or `"zone"`
#'
#' @return UTM
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' DD_to_UTM(x = -63, y = 44, return = "all")
#' DD_to_UTM(x = -63, y = 44, return = "easting")
#' DD_to_UTM(x = -63, y = 44, return = "northing")
#' DD_to_UTM(x = -63, y = 44, return = "zone")
#' @export
DD_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  #### Stop Conditions 
  if(!is.numeric(x)) stop("x must be numeric")
  if(!is.numeric(y)) stop("y must be numeric") #DQ think this is where it gets held up and gives the error
  if(!return %in% c("all", "easting", "northing", "zone")) stop("return must be one of all, easting, northing, or zone")
  
  xy <- data.frame(easting = x, northing = y)
  
  sp::coordinates(xy) <- c("easting", "northing")
  sp::proj4string(xy) <- sp::CRS(paste0("+proj=longlat + datum=", ellipsoid))
  
  zone <- dplyr::case_when(
    y >= 56 & y < 64 & x >= 3 & x < 6 ~ 32,
    y >= 72 & x >= 6  & x < 9 ~ 31,
    y >= 72 & x >= 9  & x < 12 ~ 33,
    y >= 72 & x >= 18 & x < 21 ~ 33,
    y >= 72 & x >= 21 & x < 24 ~ 35,
    y >= 72 & x >= 30 & x < 33 ~ 35,
    y >= 72 & x >= 33 & x < 42 ~ 37,
    x >= -180 & x <= 180 ~ (floor((x + 180)/6) %% 60) + 1,
    TRUE ~ NA_real_)
  
  res <- sp::spTransform(xy, sp::CRS(paste0("+proj=utm +zone=", zone, " ellps=", ellipsoid))) %>%
    as.data.frame() %>% 
    set_names(c("easting", "northing")) %>% 
    dplyr::select(easting, northing) %>%
    dplyr::mutate(zone = zone)
  
  if(return == "all") {
    return(stats::setNames(as.list(c(res$easting, res$northing, res$zone)),
           c("easting", "northing", "zone")))}
  else {
    return(res %>% dplyr::pull(tidyselect::all_of(return)))
  }
  
}
