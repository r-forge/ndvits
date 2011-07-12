\name{ExtractVGT}
\alias{ExtractVGT}
\title{
Extract time series from VITO Vegetation data
}
\description{
Extract time series from VITO Vegetation data
}
\usage{
ExtractVGT(shapefile, shapedir, ndvidirectory, region, outfile = "TS.txt", Ystart, Yend, ext = "shp")
}
\arguments{
  \item{shapefile}{name of the shapefile (multipoints or polygons).\cr
For a shapefile called "name.shp" or a kml file called "name.kml", write "name".}
  \item{shapedir}{direction of the folder containing the shapefile.
Be careful, for windows, the direction in R use double  \\\\ instaed of \\.
For a shapefile in "C:\\Dir\\name.shp" , write "C:\\\\Dir\\\\".
For a kml file in "C:\\Dir\\name.kml" , write "C:\\\\Dir\\\\name.kml" .
}
  \item{ndvidirectory}{directory of the SPOT VGT ndvi images (geotiff)}
  \item{region}{name of the region (same as in the ndvi tif images).}
  \item{outfile}{file where the time serie is saved (extenstion ".txt"). By default, outfile="TS.txt".}
  \item{Ystart}{year of start of the time series available.}
  \item{Yend}{year of end of the time series available.}
  \item{ext}{extension of the points/polygons file : "shp" or "kml".
By default, ext="shp".}
}
\details{
FreeVGT VITO's nomenclature : (type=VITO_VGT)\cr
\verb{NDV_[YYYYMMDD]_[REGION]_Extract.tif}
\itemize{
\item{YYYY}{corresponging to the year (four digits)}
\item{MM}{corresponging to the months (two digits)}
\item{DD}{corresponging to the date of the period : "01", "11" or "21" (10-day mesures).}
\item{REGION}{the region selected while extracting maps}
}
}
\value{
Return the time series of the points/polygons in the shapefile over the desired period.
}
\references{
VGT ref
}
\author{
Romain Frelat and Bruno Gerard
}
\seealso{
\code{\link{TimeSeriesAnalysis}}, \code{\link{ClipVGT}}
}
\keyword{ts extraction}
