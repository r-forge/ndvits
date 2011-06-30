\name{ExtractGIMMS}
\alias{ExtractGIMMS}
\title{Extract time series from GIMMS data}
\description{Extract time series from GIMMS data.}
\usage{ExtractGIMMS(shapefile, shapedir, ndvidirectory, region, outfile, Ystart, Yend, ext = "shp")}
\arguments{
  \item{shapefile}{name of the shapefile (multipoints or polygons).\cr
For a shapefile called "name.shp" or a kml file called "name.kml", write "name".}
  \item{shapedir}{direction of the folder containing the shapefile.
Be careful, for windows, the direction in R use double  \\\\ instaed of \\.
For a shapefile in "C:\\Dir\\name.shp" , write "C:\\\\Dir\\\\".
For a kml file in "C:\\Dir\\name.kml" , write "C:\\\\Dir\\\\name.kml" .}
  \item{ndvidirectory}{directory of the GIMMS ndvi images (geotiff)}
  \item{region}{name of the region (same as in the ndvi tif images).}
  \item{outfile}{file where the time serie is saved (extenstion ".txt"). By default, outfile="TS.txt".}
  \item{Ystart}{year of start of the time series available.}
  \item{Yend}{year of end of the time series available.}
  \item{ext}{extension of the points/polygons file : "shp" or "kml".
By default, ext="shp".}
}
\details{
GIMMS nomenclature (from Global Land Cover Facility) : (type=GIMMS)\cr
\verb{[RE][YY][MMM][DDD].[SAT]-VIg.tif}
\itemize{
\item{RE}{identifies the continent (AF for Africa,AZ for Australia and New Zealand, EA for Eurasia, NA for North America and SA for South America and Central America)}
\item{YY}{corresponging to the year (two last digits)}
\item{MMM}{abbreviation of the month (three letters)}
\item{DDD}{identifies the composite period, 15a denotes the days 1-15 of the month, and 15b denotes the days from 16 to the end of the month.}
\item{SAT}{identifies the satellite, NOAA-7, 9, 11, 14, 16, or 17 from which the data originated.}
}
}
\value{Return the time series of the points/polygons in the shapefile over the desired period.}
\author{Romain Frelat and Bruno Gerard}
