\name{multimap}
\alias{multimap}
\title{Display multi-map.}
\description{Display multi map in R.}
\usage{
multimap(listfiles, names, namefile, org, ext = "show", main = "maps", obj = NULL, objdir = NULL, pal = "Spectral", objext = "shp", ...)
}
\arguments{
  \item{listfiles}{vector with names and directory of maps to be plotted together (better to load geotiff files).}
  \item{names}{names of the different maps (display at the top of them).}
  \item{namefile}{name of the output map}
  \item{org}{organisation of the maps : vector with number of rows, number of columns.}
  \item{ext}{display of the map : "show", "jpg", "png" or "pdf"}
  \item{main}{title of the map}
  \item{obj}{obj : name of the file with objects (point or polygon) to display on the maps (shapefile or kml).
if  \verb{obj=NULL}, no object will be added to the anomaly maps. By default, \verb{obj=NULL}.}
  \item{objdir}{objdir :  direction of the folder containing the file with the objects.}
  \item{pal}{palette of colors used to make the map (see note 1.5 : choosing the palette)}
  \item{objext}{objext : extension of the objects ("shp" or "kml")
By default, \verb{objext="shp"}}
  \item{\dots}{optional arguments send to spplot}
}
\author{Romain Frelat and Bruno Gerard.}
\seealso{
\code{\link{savemap}}
}
\keyword{plot}
