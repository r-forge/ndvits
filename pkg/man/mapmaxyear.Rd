\name{mapmaxyear}
\alias{mapmaxyear}
\title{Maximum NDVI value of the year}
\description{Compute for each pixel, the maximum NDVI value of the year.}
\usage{
mapmaxyear(ndvidirectory, region, year, xlim = NULL, ylim = NULL, fileout = FALSE, type="VITO_CLIP")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi images (geotiff)}
  \item{region}{name of the region (same as in the ndvi tif images)}
  \item{year}{Year of the analysis}
  \item{xlim}{longitude of the desired area}
  \item{ylim}{latitude of the desired area}
  \item{fileout}{file where the map of the maximum is saved. If fileout=FALSE, the map is not saved.}
  \item{type}{type of data : GIMMS, VITO_VGT or VITO_CLIP}
}
\details{
\verb{mapmaxyear} load one maps after another and keep the maximum. It is less memory and time demanding than maplocalstat.
}
\value{
The function returns the map with the maximum of the year.
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{maplocalstat}}
}
