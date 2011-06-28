\name{maplocalstat}
\alias{maplocalstat}
\title{NDVI local statistics}
\description{
Compute statistics of NDVI maps of a certain time.
}
\usage{
maplocalstat(ndvidirectory, region, Ystart, Yend, xlim = NULL, ylim = NULL, type="VITO_CLIP")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi images (geotiff)}
  \item{region}{name of the region (same as in the ndvi tif images)}
  \item{Ystart}{Year of start of the analysis}
  \item{Yend}{Year of end of the analysis}
  \item{xlim}{longitude of the desired area}
  \item{ylim}{latitude of the desired area}
  \item{type}{type of data : GIMMS, VITO_VGT or VITO_CLIP}
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
\code{\link{mapmaxyear}}
}
