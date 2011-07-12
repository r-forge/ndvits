\name{savemap}
\alias{savemap}
\title{
Save a map.
}
\description{
Save a given map in the desire format
}
\usage{
savemap(map, obj = NULL, shapedir = NULL, ext = "show", namefile = "map", pal = "Spectral", objext = "shp", label = FALSE)
}
\arguments{
  \item{map}{map to save}
  \item{obj}{name of the shapefile/kml file with the objects (points or polygon) to plot over the image}
  \item{shapedir}{direction of the object}
  \item{ext}{ext : extension of the file ("png", "jpg" or "pdf")\cr
If ext="show", the map is not saved but only displayed in R. By default, ext="show".}
  \item{namefile}{name of the file (without extension)}
  \item{pal}{pal : palette of colors used to make the map (see note 1.5 : choosing the palette)}
  \item{objext}{extension of the objects ("shp" or "kml")}
  \item{label}{label : name of the label of the points.If \verb{label=FALSE}, the label will not be plotted.}
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{multimap}}
}
\keyword{plot}
