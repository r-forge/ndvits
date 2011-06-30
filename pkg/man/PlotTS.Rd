\name{PlotTS}
\alias{PlotTS}
\title{
The function plots the time series one under another.
}
\description{
Time series are plotted one under another with the same scale of time.
}
\usage{
PlotTS(TS, fileout = FALSE, Ystart, period, title = "NDVI time series")
}
\arguments{
  \item{TS}{Time serie or matrix of time serie containing only NDVI data (can be a single time series or multiple). }
  \item{fileout}{file where to save the graph. If FALSE (value by default), plot are shown in R.}
  \item{Ystart}{start year of the time series.}
  \item{period}{number of observations per year (for vito: period=36, for gimms: period=24).}
  \item{title}{title of the graphs. By default, title="NDVI time series".}
}
\author{
Romain Frelat and Bruno Gerard
}

