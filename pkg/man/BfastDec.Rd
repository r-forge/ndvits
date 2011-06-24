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
}
\value{
to be explained
res$seas
res$trend
}
\references{
Verbesselt 2 articles
}
\author{
Romain Frelat and Bruno Gerard
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}