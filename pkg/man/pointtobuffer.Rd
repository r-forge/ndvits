\name{pointtobuffer}
\alias{pointtobuffer}
\title{
Point to buffer
}
\description{
Create a shapefile of points around the points given in the kml/shapefile.
}
\usage{
pointtobuffer(shapefile, shapedir, ndvidirectory, region, Ystart, ext="shp", nameshape = "buffer", dirshape = ".", rad = 1, type="VITO_CLIP")
}
\arguments{
  \item{shapefile}{name of the shapefile (multipoints)}
  \item{shapedir}{direction of the folder containing the shapefile.}
  \item{ndvidirectory}{directory of the ndvi files (to know the spatial resolution).}
  \item{region}{name of the region}
  \item{Ystart}{year where data is available}
  \item{ext}{extension of the shape/kml file}
  \item{nameshape}{Name of the new shapefile}
  \item{dirshape}{Directory of the new shapefile}
  \item{rad}{radius around the points (in pixel)}
  \item{type}{type of ndvi data (VITO_CLIP, VITO_VGT or GIMMS)}
}
\value{
Nothing is return, a shapefile is created and saved in the folder \verb{dirshape}, under the name \verb{nameshape}.
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{polytopoints}}
}
\keyword{spatial}
