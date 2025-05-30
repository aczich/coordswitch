#' DMS_to_UTM
#'
#' @param x latitude in degrees minutes seconds (character); e.g. `"63° 0' 0\" E"`
#' @param y longitude in degrees minutes seconds (character); e.g. `"44° 0' 0\" W"`
#' @param ellipsoid reference ellipsoid (character); defaults to `"WGS40"`
#' @param return desired output; one of `"all"`, `"northing"`, `"easting"`, or `"zone"`
#'
#' @return UTM
#'
#' @examples
#' DMS_to_UTM(x = "63° 0' 0\" E", y = "44° 0' 0\" W", return = "all")
#' DMS_to_UTM(x = "63° 0' 0\" E", y = "44° 0' 0\" W", return = "northing")
#' DMS_to_UTM(x = "63° 0' 0\" E", y = "44° 0' 0\" W", return = "easting")
#' DMS_to_UTM(x = "63° 0' 0\" E", y = "44° 0' 0\" W", return = "zone")
#' @export
DMS_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  x <- DMS_to_DD(x, axis = "horizontal")
  y <- DMS_to_DD(y, axis = "vertical")
  
  DD_to_UTM(x, y, ellipsoid, return)
  
}
