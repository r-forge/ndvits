\name{AnPhenoMetrics}
\alias{AnPhenoMetrics}
\title{
Extracting annual phenological metrics
}
\description{
This function extracts annual phenological metrics
}
\usage{
AnnualPhenologicalMetrics(TS, fileout, outgraph, Ystart, period, SOSth = 0.5, EOSth = 0.5)
}
\arguments{
  \item{TS}{time serie or matrix of time serie containing only NDVI data (can be a single time series or multiple). }
  \item{fileout}{ txt file where metrics are saved. }
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
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
mainly Vrieling 2011.
}
\author{
Romain Frelat and Bruno Gerard
}
\note{
%%  ~~further notes~~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
