% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_DD_to_UTM.R
\name{DD_to_UTM}
\alias{DD_to_UTM}
\title{DD_to_UTM}
\usage{
DD_to_UTM(x, y, ellipsoid = "WGS84", return = "all")
}
\arguments{
\item{x}{latitude in decimal degrees (numeric)}

\item{y}{longitude in decimal degrees (numeric)}

\item{ellipsoid}{reference ellipsoid (character); defaults to \code{"WGS40"}}

\item{return}{desired output; one of \code{"all"}, \code{"northing"}, \code{"easting"}, or \code{"zone"}}
}
\value{
UTM
}
\description{
DD_to_UTM
}
\examples{
DD_to_UTM(x = -63, y = 44, return = "all")
DD_to_UTM(x = -63, y = 44, return = "easting")
DD_to_UTM(x = -63, y = 44, return = "northing")
DD_to_UTM(x = -63, y = 44, return = "zone")
}
