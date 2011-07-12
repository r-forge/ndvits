\name{shapelim}
\alias{shapelim}
\title{
limits of a shape/kml file
}
\description{
Find the limit of a shape/kml file}
\usage{
shapelim(shape, shapedir, ext = "shp", around = 0.05)
}
\arguments{
  \item{shape}{
name of the shape/kml file
}
  \item{shapedir}{
folder containing the shapefile
}
  \item{ext}{
type of file ("shp" or "kml"). By default, ext="shp".
}
  \item{around}{
extra space (in degree) kept around the shapefile. By default, around=0.05 .
}
}
\value{
Return an object with :
\item{xlim}{a vector with the longitude borders of the shapefile}
\item{ylim}{a vector with the latitude borders of the shapefile}
\item{proj}{the projection of the shapefile}
}
\author{
Romain Frelat and Bruno Gerard
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{spatial}
