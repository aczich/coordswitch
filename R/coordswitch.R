#' Convert coordinates in a data frame
#'
#' Use existing spatial data in a data frame to convert to other coordinate systems.
#'
#' @param data A data frame.
#' @param x The name of the variable representing the horizontal plane; defaults to lon.
#' @param y The name of the variable representing the vertical meridian; defaults to lat.
#' @param from The initial coordinate system. One of DD, DMS, DDM, or UTM. Defaults to DD.
#' @param to The initial coordinate system. One of DD, DMS, DDM, or UTM, or all. Defaults to all.
#' @param zone If converting from UTM, the name of the numeric variable containing UTM zone; defaults to zone.
#' @param ellipsoid The ellipsoid ID. Defaults to WGS84.
#'
#' @return The original data frame with converted coordinates appended as new columns
#'
#' @importFrom rlang :=
#'
#' @examples
#' df_dd <- data.frame(lon = c( -63, -89.2345, 4.5, 4.5), lat = c(44, 23.34, 57.3246, 57.3246))
#' sp_convert(df_dd, from = "DD", to = "DDM")
#' 
#' df_utm <- sp_convert(df_dd, from = "DD", to = "UTM")
#' sp_convert(df_utm, x = "easting", y = "northing", from = "UTM", to = "DDM")
#' 
#' @export
coordswitch <- function(data, x = "lon", y = "lat",
                       from = "DD", to = "all",
                       zone = "zone", ellipsoid = "WGS84") {
  #### Stop Conditions 
  if(!is.data.frame(data)) stop("data object must be a data frame")
  if(!x %in% names(data)) stop("x argument does not match any variables in data")
  if(!y %in% names(data)) stop("y argument does not match any variables in data")
  if(length(to) == 1) {if(from == to) stop("from argument matches to argument; no conversion required")}
  if(!from %in% c("DD", "DMS", "DDM", "UTM")) stop("Invalid from argument; please specify DD, DMS, DDM, or UTM")
  if(!from %in% c("all", "DD", "DMS", "DDM", "UTM")) stop("Invalid from argument; please specify all, DD, DMS, DDM, or UTM")
  if(from == "UTM" & !zone %in% names(data)) stop("zone variable missing from data")
  list.files("R", pattern = "function", full.names = T) %>% walk(source)
  #### Create Reference Table 
  distinctData <- data %>%
    dplyr::rename(x = tidyselect::all_of(x), y = tidyselect::all_of(y)) %>%
    tidyr::drop_na(x, y) %>%
    # If UTM is being used, rename zone variable
    {if(from == "UTM") dplyr::rename(., zone = tidyselect::all_of(zone)) else .} %>%
    # Keep unique combinations of 'x' and 'y'; if 'from' is UTM, also keep 'zone'
    {if(from == "UTM") dplyr::distinct(., x, y, zone) else dplyr::distinct(., x, y)}
  
  #### Identify Conversion Functions 
  # If `to` value is "all", specify all options
  if("all" %in% to) {to <- c("DD", "DDM", "DMS", "UTM")}
  
  # Remove matching 'from' and 'to' values
  if(from %in% to) to <- to[!to == from]
  
  # Identify conversion functions required using from and to arguments
  use_functions <- stats::setNames(as.list(paste(from, "to", to, sep = "_")), to)
  non_utm_functions <- use_functions[!stringr::str_detect(use_functions, "UTM")]
  to_utm_functions <- use_functions[stringr::str_detect(use_functions, "to_UTM")]
  from_utm_functions <- use_functions[stringr::str_detect(use_functions, "UTM_to")]
  
  # Convert character names to actual functions for if else in pipe
  non_utm_fun <- map(non_utm_functions, get)
  to_utm_fun <- map(to_utm_functions, get)
  from_utm_fun <- map(from_utm_functions, get)
  
  
  
  #### Apply Conversion Functions 
  coords_data <- distinctData %>%
    dplyr::rowwise() %>%
    # Apply any functions associated with non-UTM conversions
    {if (length(non_utm_functions) > 0)
      dplyr::mutate(.,
                    lon_DDM = DD_to_DDM(x, axis = "horizontal"),
                    lat_DDM = DD_to_DDM(y, axis = "vertical"),
                    lon_DMS = DD_to_DMS(x, axis = "horizontal"),
                    lat_DMS = DD_to_DMS(y, axis = "vertical"))
      else .} %>%
    # Apply any functions associated with conversions 'to' UTM
    {if (length(to_utm_functions) > 0)
      {if(to_utm_functions %in% "DD_to_UTM"){
      dplyr::mutate(.,
                    easting = DD_to_UTM(x, y, return = "easting"),
                    northing = DD_to_UTM(x, y, return = "northing"),
                    zone = DD_to_UTM(x, y, return = "zone"))
    }else if(to_utm_functions %in% "DDM_to_UTM"){
      dplyr::mutate(.,
                    easting = DDM_to_UTM(x, y, return = "easting"),
                    northing = DDM_to_UTM(x, y, return = "northing"),
                    zone = DDM_to_UTM(x, y, return = "zone"))
    }else if(to_utm_functions %in% "DMS_to_UTM"){
      dplyr::mutate(.,
                    easting = DMS_to_UTM(x, y, return = "easting"),
                    northing = DMS_to_UTM(x, y, return = "northing"),
                    zone = DMS_to_UTM(x, y, return = "zone"))
    } 
      }else .} %>%
    # Apply any functions associated with conversions 'from' UTM
    {if (length(from_utm_functions) > 0)
      {if(from_utm_functions %in% "UTM_to_DD"){
        dplyr::mutate(.,
                      lon_DD = UTM_to_DD(x, y, zone = zone, return = "lon"),
                      lat_DD = UTM_to_DD(x, y, zone = zone, return = "lat"))
      }else if(from_utm_functions %in% "UTM_to_DDM"){
        dplyr::mutate(.,
                      lon_DDM = UTM_to_DDM(x, y,  zone = zone, return = "lon"),
                      lat_DDM = UTM_to_DDM(x, y,  zone = zone, return = "lat"))
      }else if(from_utm_functions %in% "UTM_to_DMS"){
        dplyr::mutate(.,
                      lon_DMS = UTM_to_DMS(x, y,  zone = zone, return = "lon"),
                      lat_DMS = UTM_to_DMS(x, y,  zone = zone,return = "lat"))
      } 
      }else .} %>%
    # Rename columns to clean up
    dplyr::rename("{paste(x, from, sep = '_')}" := x,
           "{paste(y, from, sep = '_')}" := y) %>%
    dplyr::rename_with(.fn = ~gsub("x_", "lon_", .), .cols = dplyr::starts_with("x_")) %>%
    dplyr::rename_with(.fn = ~gsub("y_", "lat_", .), .cols = dplyr::starts_with("y_"))
  
  #### Join and Return 
  data %>%
    dplyr::rename("{paste(x, from, sep = '_')}" := tidyselect::all_of(x), 
           "{paste(y, from, sep = '_')}" := tidyselect::all_of(y)) %>%
    dplyr::left_join(coords_data)
}
