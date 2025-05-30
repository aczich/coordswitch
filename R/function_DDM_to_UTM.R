#' DDM_to_UTM
#'
#' @param x latitude in degrees decimal minutes (character); e.g. `"63° 0 W"`
#' @param y longitude in degrees decimal minutes (character); e.g. `"44° 0 N"`
#' @param ellipsoid reference ellipsoid (character); defaults to `"WGS40"`
#' @param return desired output; one of `"all"`, `"northing"`, `"easting"`, or `"zone"`
#'
#' @return UTM
#'
#' @examples
#' DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "all")
#' DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "northing")
#' DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "easting")
#' DDM_to_UTM(x = "63° 0 W", y = "44° 0 N", return = "zone")
#' 
#' @export
DDM_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  x <- DDM_to_DD(x, axis = "horizontal")
  y <- DDM_to_DD(y, axis = "vertical")

  DD_to_UTM(x, y, ellipsoid, return)
  
}
