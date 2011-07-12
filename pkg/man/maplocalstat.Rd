\name{maplocalstat}
\alias{maplocalstat}
\title{NDVI local statistics}
\description{
Compute statistics of NDVI maps of a certain time.
}
\usage{
maplocalstat(ndvidirectory, region, Ystart, Yend, xlim = NULL, ylim = NULL, type="VITO_CLIP", outname = "stat", org=c(2,2), obj=NULL, objdir=NULL, objext="shp", ext="show", pal = "Spectral")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi images (geotiff)}
  \item{region}{name of the region (same as in the ndvi tif images)}
  \item{Ystart}{Year of start of the analysis}
  \item{Yend}{Year of end of the analysis}
  \item{xlim}{longitude of the desired area}
  \item{ylim}{latitude of the desired area}
  \item{type}{type of data : GIMMS, VITO_VGT or VITO_CLIP}
  \item{outname}{prefixe of the saved name}
  \item{org}{organisation of the maps. To change only if maps are not cube then can be c(1,3) or c(3,1).}
  \item{obj}{object to show on the maps}
  \item{objdir}{direction of the object to show on the maps}
  \item{objext}{extension of the object file : "kml" or "shp"}
  \item{ext}{how is saved the map}
  \item{pal}{palette}
}
\details{
\verb{maplocalstat} load the maps between \verb{Ystart} and \verb{Yend}. Then it computes basics statistics (max, min, mean, sd) discarding NA values.
}
\value{
The function returns 4 maps containing the statistics
\item{mean }{The mean of the NDVI values between \verb{Ystart} and \verb{Yend}}
\item{max }{The maximum of the NDVI values between \verb{Ystart} and \verb{Yend}}
\item{min }{The minimum of the NDVI values between \verb{Ystart} and \verb{Yend}}
\item{sd }{The standard deviation of the NDVI values between \verb{Ystart} and \verb{Yend}}
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{mapmaxyear}},\code{\link{PeriodAnomaly}},\code{\link{MaxAnomaly}}
}
\keyword{spatial}
