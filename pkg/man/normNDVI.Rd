\name{normNDVI}
\alias{normNDVI}
\title{compute NDVI value}
\description{
Compute NDVI values (between 0 and 1) depending on the coding systems of the maps.
}
\usage{
normNDVI(TS, maxNDVI)
}
\arguments{
  \item{TS}{time series extracted}
  \item{maxNDVI}{maximum of the NDVI values (depending on how they are coded).
For VITO maxNDVI=255, for GIMMS maxNDVI=10000.}
}
\details{
\deqn{\frac{TS}{maxNDVI}}
}
\value{The output of normNDVI is a data-frame with the normalized time series.}
\author{Romain Frelat and Bruno Gerard}
\seealso{
\code{\link{TimeSeriesAnalysis}}
}
\keyword{tools}
