\name{PeriodAnomaly}
\alias{PeriodAnomaly}
\title{
Period Anomaly map
}
\description{
The function PeriodAnomaly compute the anomaly map on a given period of time.
}
\usage{
PeriodAnomaly(ndvidirectory, region, Ystart, Yend, period, namefile = "anomaly", ext = "show", xlim = NULL, ylim = NULL, type="VITO_CLIP", obj = NULL, objdir = NULL, objext = "shp", label = FALSE, pal = "Spectral")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi files}
  \item{region}{name of the region}
  \item{Ystart}{year of start of the analysis}
  \item{Yend}{year of end}
  \item{period}{period to focus}
  \item{namefile}{prefixe of the name of the file. By default namefile="anomaly"}
  \item{ext}{extension of the map}
  \item{xlim}{longitude of the desired area}
  \item{ylim}{latitude of the desired area}
  \item{type}{type of data : GIMMS, VITO_VGT or VITO_CLIP}
  \item{obj}{shapefile to draw on the map}
  \item{objdir}{direction of the shapefile to draw on the map}
  \item{objext}{extension of the shapefile ("shp" or "kml"}
  \item{label}{name of the label of the points. If label=FALSE, the label will not be plotted. By default, label=FALSE.}
  \item{pal}{palette of colors used to make the map (see note 1.5 : choosing the palette). By default, pal="Spectral".}
}
\details{
explain more here
}
\author{Romain Frelat and Bruno Gerard}
