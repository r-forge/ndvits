\name{TimeSeriesAnalysis}
\alias{TimeSeriesAnalysis}
\title{NDVI Time Series Extraction and Analysis}
\description{
Extract NDVI time series from different data source.
Group points or polygon together, compute savitzky-Golay filter.
Also exectute a basic analysis (STL decomposition).
}
\usage{
TimeSeriesAnalysis(shapefile, shapedir, ndvidirectory, region, Ystart, Yend, outfile = "TS.txt", outfile2 = "TS.pdf", outfile3 = FALSE, ext = "shp", fct = "mean", SGfilter = TRUE, nSG = "5,5", DSG = 0, title = "NDVI time series", type = "VITO_CLIP")
}
\arguments{
  \item{shapefile}{
name of the shapefile (multipoints or polygons).\cr
For a shapefile called "name.shp" or a kml file called "name.kml", write "name".
}
  \item{shapedir}{
direction of the folder containing the shapefile.
Be careful, the direction in R use double  \\\\
For a shapefile in "C:\\Dir\\name.shp" , write "C:\\\\Dir\\\\".
For a kml file in "C:\\Dir\\name.kml" , write "C:\\\\Dir\\\\name.kml" .
}
  \item{ndvidirectory}{
directory of the ndvi images (geotiff)
}
  \item{region}{
name of the region (same as in the ndvi tif images)
}
  \item{Ystart}{
year of start of the time series available
}
  \item{Yend}{
year of end of the time series available
}
  \item{outfile}{
file where the time serie is saved (extenstion ".txt")\\
By default, outfile="TS.txt"
}
  \item{outfile2}{
file where the seasonal decomposotion of the time serie is saved (extenstion ".pdf"). By default, outfile2="TS.pdf". }
  \item{outfile3}{
 file where the graphs of the different time serie (stocked) is saved (extenstion ".pdf").
If outfile3=FALSE, graphs are displayed in R (but not saved). By default, outfile3=FALSE
}
  \item{ext}{
extension of the points/polygons file : "shp" or "kml".
By default, ext="shp".
}
  \item{fct}{
function applied while grouping points of a same categorie together : "mean","max" or "min".
By default, fct="mean".
}
  \item{SGfilter}{
TRUE to apply a Savitzky-Golay filter to the time series, FALSE otherwize.
By default, SGfilter=TRUE.
}
  \item{nSG}{
moving window of the Savitzky-Golay filter.
By default, nSG="5,5" (symetric windows taking into account 5 points backward and 5 points forward.
}
  \item{DSG}{
polynomial degree of the function use in the Savitzky-Golay filter.
By default, DSG=0.
}
  \item{title}{
 title of the graph (figure 2, saved in outfile3)
By default, title="NDVI time series".
}
  \item{type}{
type of data. VITO_CLIP,VITO_VGT,GIMMS, TEXT or FILES
}
}
\value{
signal : Time series.
}
\references{
none
}
\author{
R. Frelat, B. Gerard
}
\seealso{
\code{\link{ExtractFile}},\code{\link{ExtractGIMMS}},\code{\link{ExtractVGT}},\code{\link{ExtractVito}},
\code{\link{normNDVI}},\code{\link{PlotTS}},\code{\link{STLperArea}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ts extraction}
