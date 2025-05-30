#' UTM_to_DD
#'
#' @param x easting (numeric)
#' @param y northing (numeric)
#' @param zone UTM zone (numeric)
#' @param ellipsoid reference ellipsoid (character); defaults to `"WGS40"`
#' @param return desired output; one of `"lat"`, `"lon"`, or `"all"`
#' 
#' @return DD latitude and/or longitude in decimal degree format
#'
#' @examples
#' UTM_to_DD(x = 271542.3, y = 2582924, zone = 16, return = "all")
#' UTM_to_DD(x = 271542.3, y = 2582924, zone = 16, return = "lat")
#' UTM_to_DD(x = 271542.3, y = 2582924, zone = 16, return = "lon")
#' @export
UTM_to_DD <-  function(x, y, zone, ellipsoid = "WGS84", return = "all") {
  
  # Stop Conditions
  if(!is.numeric(x)) stop("x argument must be numeric")
  if(!is.numeric(y)) stop("y argument must be numeric")
  if(!is.numeric(zone)) stop("zone argument must be numeric")
  if(!return %in% c("lat", "lon", "all")) stop("Invalid return argument; please specify lat, lon, or all")
  
  utm <- data.frame(lon = x, lat = y)
  sp::coordinates(utm) <- ~lon+lat
  sp::proj4string(utm) <- sp::CRS(paste0("+proj=utm +zone=", zone, " +ellps=", ellipsoid))
  utm <- sp::spTransform(utm, sp::CRS(paste0("+proj=longlat +datum=", ellipsoid))) %>%
    as.data.frame() %>%
    purrr::set_names(c("lon", "lat"))
  
  if(return == "all") {
    return(data.frame(lat_DD = utm$lat, lon_DD = utm$lon))
  } else {
    return(utm %>% dplyr::pull(tidyselect::all_of(return)))
  }
}
