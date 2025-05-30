% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordswitch.R
\name{coordswitch}
\alias{coordswitch}
\title{Convert coordinates in a data frame}
\usage{
coordswitch(
  data,
  x = "lon",
  y = "lat",
  from = "DD",
  to = "all",
  zone = "zone",
  ellipsoid = "WGS84"
)
}
\arguments{
\item{data}{A data frame.}

\item{x}{The name of the variable representing the horizontal spatial coordinate; defaults to lon.}

\item{y}{The name of the variable representing the horizontal spatial coordinate; defaults to lat.}

\item{from}{The initial coordinate system. One of DD, DMS, DDM, or UTM. Defaults to DD.}

\item{to}{The initial coordinate system. One of DD, DMS, DDM, or UTM, or all. Defaults to all.}

\item{zone}{If converting from UTM, the name of the numeric variable containing UTM zone; defaults to zone.}

\item{ellipsoid}{The ellipsoid ID. Defaults to WGS84.}
}
\value{
The original data frame with converted coordinates appended as new columns
}
\description{
Use existing spatial data in a data frame to convert to other coordinate systems.
}
\examples{
df_dd <- data.frame(lon = c( -63, -89.2345, 4.5, 4.5), lat = c(44, 23.34, 57.3246, 57.3246))
coordswitch(df_dd, from = "DD", to = "DDM")

df_utm <- coordswitch(df_dd, from = "DD", to = "UTM")
coordswitch(df_utm, x = "easting", y = "northing", from = "UTM", to = "DDM")

}
