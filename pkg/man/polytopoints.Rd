\name{polytopoints}
\alias{polytopoints}
\title{
Polynoms to Points
}
\description{
From a polygon shapefile, create a shapefile of points.
}
\usage{
polytopoints(shapefile, shapedir, ndvidirectory, region, Ystart, ext = "shp", type = "VITO_CLIP", nameshape = "buffer", dirshape = ".")
}
\arguments{
  \item{shapefile}{name of the shapefile (multipoints)}
  \item{shapedir}{direction of the folder containing the shapefile.}
  \item{ndvidirectory}{directory of the ndvi files (to know the spatial resolution).}
  \item{region}{name of the region}
  \item{Ystart}{year where data is available}
  \item{ext}{extension of the shape/kml file}
  \item{type}{type of ndvi data (VITO_CLIP, VITO_VGT or GIMMS)}
  \item{nameshape}{Name of the new shapefile}
  \item{dirshape}{Directory of the new shapefile}
}
\value{
Nothing is return, a shapefile is created and saved in the folder \verb{dirshape}, under the name \verb{nameshape}.
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{pointtobuffer}}
}
\keyword{spatial}
