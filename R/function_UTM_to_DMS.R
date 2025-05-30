#' UTM_to_DMS
#'
#' @param x easting (numeric)
#' @param y northing (numeric)
#' @param zone UTM zone (numeric)
#' @param ellipsoid reference ellipsoid (character); defaults to `"WGS40"`
#' @param return desired output; one of `"lat"`, `"lon"`, or `"all"`
#' 
#' @return DMS latitude and/or longitude in degree minute second format
#'
#' @examples
#' UTM_to_DMS(x = 271542.3, y = 2582924, zone = 16, return = "all")
#' UTM_to_DMS(x = 271542.3, y = 2582924, zone = 16, return = "lat")
#' UTM_to_DMS(x = 271542.3, y = 2582924, zone = 16, return = "lon")
#' 
#' @export
UTM_to_DMS <-  function(x, y, zone, ellipsoid = "WGS84", return = "all") {
  
  # Stop Conditions
  if(!is.numeric(x)) stop("x argument must be numeric")
  if(!is.numeric(y)) stop("y argument must be numeric")
  if(!is.numeric(zone)) stop("zone argument must be numeric")
  if(!return %in% c("lat", "lon", "all")) stop("Invalid return argument; please specify lat, lon, or all")
  
  if(return == "all") {
    latval <- DD_to_DMS(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return = "lat"),
                        axis = "vertical")
    lonval <- DD_to_DMS(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return = "lon"),
                        axis = "horizontal")
    return(stats::setNames(as.list(c(latval, lonval)),
                    c("lat_DMS", "lon_DMS")))
    
    
  } else {
    DD_to_DMS(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return),
              ifelse(return == "lon", "horizontal", "vertical"))  
  }
  
}
