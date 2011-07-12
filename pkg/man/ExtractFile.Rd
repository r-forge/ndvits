\name{ExtractFile}
\alias{ExtractFile}
\title{
Extract time series from a list of images
}
\description{
Extract time series from a file containing a list of images
}
\usage{
ExtractFile(shapefile, shapedir, listfile, outfile, period, ext = "shp")
}
\arguments{
  \item{shapefile}{name of the shapefile (multipoints or polygons).\cr
For a shapefile called "name.shp" or a kml file called "name.kml", write "name".}
  \item{shapedir}{direction of the folder containing the shapefile.
Be careful, for windows, the direction in R use double  \\\\ instaed of \\.
For a shapefile in "C:\\Dir\\name.shp" , write "C:\\\\Dir\\\\".
For a kml file in "C:\\Dir\\name.kml" , write "C:\\\\Dir\\\\name.kml" .}
  \item{listfile}{file containing a list of images}
  \item{outfile}{file where the time serie is saved (extenstion ".txt"). By default, outfile="TS.txt".}
  \item{period}{number of observation per year}
  \item{ext}{extension of the points/polygons file : "shp" or "kml".
By default, ext="shp".}
}
\details{
\var{listfile} should respect the following format :
\verb{N}
\verb{path\image1.tif}
\verb{path\image2.tif}
\verb{...}
\verb{path\imageN.tif}
with \verb{N} the number of image files. 

The list of images should be in chronological order, and include the complete path to the file, unless the files are located in the working directory from which R is run. Only the N first images in the file will be processed.
}
\value{Return the time series of the points/polygons in the shapefile over images in the shapefile.}
\author{Romain Frelat and Bruno Gerard}
\section{Warning }{It is the responsability of the user to have complete and ordered dataset : list of images have to be in chronological order and without missing image.}
\seealso{\code{\link{TimeSeriesAnalysis}},\code{\link{tolist}}}
\keyword{ts extraction}
