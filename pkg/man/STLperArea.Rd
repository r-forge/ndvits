\name{STLperArea}
\alias{STLperArea}
\title{Seasonal Decomposition per Area}
\description{
Make a Seasonal Decomposition of Time Series by Loess per group of pixels
}
\usage{
STLperArea(TS, area, fileout = FALSE, Ystart, period = 36, fct = "mean", SGfilter = TRUE, nSG = "5,5", DSG = 0)
}
\arguments{
  \item{TS}{a matrix containing only NDVI data, one row per pixel}
  \item{area}{factor which group pixels of the same area together}
  \item{fileout}{pdf file where graphs will be saved}
  \item{Ystart}{start year of the ndvi data}
  \item{period}{ number of observation per year}
  \item{fct}{ function applied to group points from the same area ("mean", "max" or "min"). By default fct="mean".}
  \item{SGfilter}{application of Savitzky-Golay filter on time series
By default SGfilter=TRUE}
  \item{nSG}{parameters of the windows of the SG filter.\cr
By default nSG="5,5" (windows 5,5).}
  \item{DSG}{parameters of the degree polynome of the SG filter. 
By default, DSG=0.}
}
\value{
The output of SeosonalDecompositionArea is the time series computed (mean/max of the time series grouped by \verb{area} and with/without Savitzky-Golay filter).
}
\author{
Romain Frelat and Bruno Gerard
}
