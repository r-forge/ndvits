\name{ExtractFile}
\alias{ExtractFile}
\title{
Extract time series from a list of images
}
\description{
Extract time series from a file containing a list of images
}
\usage{
ExtractFile(shapefile, shapedir, listfile, outfile, ext = "shp")
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
  \item{ext}{extension of the points/polygons file : "shp" or "kml".
By default, ext="shp".}
}
\details{
More about TIMESAT nomeenclature
}
\value{Return the time series of the points/polygons in the shapefile over images in the shapefile.}
\author{Romain Frelat and Bruno Gerard}
\note{It is the responsability of the user to have complete dataset ...}
