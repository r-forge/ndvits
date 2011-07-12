\name{periodtoMap}
\alias{periodtoMap}
\title{
Period to name of the NDVI map.
}
\description{
Return the complete directory of the map of a desired period.
}
\usage{
periodtoMap(ndvidirectory, region, year, period, type = "VITO_CLIP")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi files}
  \item{region}{name of the region}
  \item{year}{year of the map to load}
  \item{period}{period of the map to load. Be careful, for GIMMS data, period between 1 and 24 while for VITO data, period is between 1 and 36.}
  \item{type}{Type of the NDVI data : choose between VITO_CLIP, VITO_VGT and GIMMS (for more information see nomenclarure).}
}
\value{
Return the complete directory of the map.
If the map doesn't exist, an error message is printed.
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{timetoMap}}
}
\keyword{tools}
