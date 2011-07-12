\name{MaxAnomaly}
\alias{MaxAnomaly}
\title{Maximum Anomaly}
\description{
The function MaxAnomaly compute the anomaly map of the difference between the maximum of each year and their mean.
}
\usage{
MaxAnomaly(ndvidirectory, region, Ystart, Yend, namefile = "anomaly", ext = "show", xlim = NULL, ylim = NULL, type="VITO_CLIP", obj = NULL, objdir = NULL, objext = "shp", label = FALSE, pal = "Spectral")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi files}
  \item{region}{name of the region}
  \item{Ystart}{year of start of the analysis}
  \item{Yend}{year of end}
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
The map of the max of each year between \verb{Ystart} and \verb{Yend} is computed. Then, the mean is calculated.  The anomaly map comes from the difference between the mean of the max and the max of the specific year.  It is saved automatically in geotiff and displayed by default (show=TRUE) in the graphic of R.  To go from one graph to another, click on the graph or press enter in the R console.
}
\author{Romain Frelat and Bruno Gerard}
\seealso{
\code{\link{maplocalstat}},\code{\link{PeriodAnomaly}}
}
\keyword{anomaly map}
