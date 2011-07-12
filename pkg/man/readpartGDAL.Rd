\name{readpartGDAL}
\alias{readpartGDAL}
\title{
partial read of GDAL map 
}
\description{
Load geotiff maps partially}
\usage{
readpartGDAL(x, xlim = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
complete directory of the map to load
}
  \item{xlim}{
longitude of the area to load.\cr
If nothing enter (xlim=NULL), the map is load entirely.
}
  \item{ylim}{
latitude of the area to load.\cr
If nothing enter (xlim=NULL), the map is load entirely.
}
  \item{\dots}{optional arguments send to readGDAL}
}
\value{
Returns the map of the desired area.
}
\author{
Romain Frelat and Bruno Gerard
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
#depending on the data available...
}
\keyword{spatial}
