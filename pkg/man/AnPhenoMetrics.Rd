\name{AnPhenoMetrics}
\alias{AnPhenoMetrics}
\title{
Extracting annual phenological metrics
}
\description{
This function extracts annual phenological metrics
}
\usage{
AnPhenoMetrics(TS, outfile, outgraph, Ystart, period, SOSth = 0.5, EOSth = 0.5)
}
\arguments{
  \item{TS}{time serie or matrix of time serie containing only NDVI data (can be a single time series or multiple). }
  \item{outfile}{ txt file where metrics are saved. }
  \item{outgraph}{pdf file where graph will be saved.}
  \item{Ystart}{start year of the ndvi data.}
  \item{period}{number of observations per year (for vito: period=36, gimms: period=24).}
  \item{SOSth}{threshold for detection start of the season.
        by default SOSth=0.5}
  \item{EOSth}{threshold for detection end of the season.
        by default EOSth=0.5}
}
\details{
How it works
the calcul in itself
how to detect double cropping season
the max min
and stuff
}
\value{
A text file is saved with phenological metrics but nothing is returned.
}
\references{
Brown, M. E. and de Beurs K. and Vrieling A. 2010 \emph{Remote Sensing of Environment}, The response of African land surface phenology to large scale climate oscillations, \bold{114}, 2286--2296
}
\author{
Romain Frelat and Bruno Gerard
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
