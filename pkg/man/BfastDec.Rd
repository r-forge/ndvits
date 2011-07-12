\name{BfastDec}
\alias{BfastDec}
\title{BFAST decomposition}
\description{
Breaks for Additive Seasonal and Trend
}
\usage{
BfastDec(TS, Ystart, period, param = c(), fileout = FALSE, ...)
}
\arguments{
  \item{TS}{
time serie or matrix of time serie containing only NDVI data (can be a single time series or multiple). 
}
  \item{Ystart}{
start year of the ndvi data.
}
  \item{period}{
number of observations per year (for vito, period=36).
}
  \item{param}{
parameters for Bfast decomposition
}
  \item{fileout}{
file where graphs are saved (extension "pdf")\\
If fileout=FALSE, the graphs are shown in R. By default, fileout=FALSE.
}
  \item{\dots}{optional arguments send to Bfast function}
}
\value{
to be explained
res$seas
res$trend
}
\references{
Verbesselt, J., Hyndman, R., Newnham, G. and Culvenor, D. 2010 \emph{Remote Sensing of Environment}, Detecting trend and seasonal changes in satellite image time series, \bold{114}, 106--115

Verbesselt, J., Hyndman, R., Zeileis, A. and Culvenor, D. 2010 \emph{Remote Sensing of Environment}, Phenological change detection while accounting for abrupt and gradual trends in satellite image time series, \bold{114}, 2970--2980
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{STLperArea}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ts decomposition}
