% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_DMS_to_DDM.R
\name{DMS_to_DDM}
\alias{DMS_to_DDM}
\title{DMS_to_DDM}
\usage{
DMS_to_DDM(DMS_input, axis)
}
\arguments{
\item{DMS_input}{latitude or longitude in degrees minute second format (character); e.g. \verb{"63° 0' 0\\" E"}}

\item{axis}{one of \code{"horizontal"} (latitude) or \code{"vertical"} (longitude)}
}
\value{
DDM
}
\description{
DMS_to_DDM
}
\examples{
DMS_to_DDM(DMS_input = "63° 0' 0\" E", axis = "horizontal")
DMS_to_DDM(DMS_input = "44° 0' 0\" N", axis = "vertical")
}
