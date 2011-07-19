\name{tolist}
\alias{tolist}
\title{To list}
\description{
Create a file containing a list with the directories of maps.
This file can be used in Timesat or with the function \code{\link{ExtractFile}}.
}
\usage{
tolist(ndvidirectory, region, Ystart, Yend, outfile = "list.txt", type = "VITO_VGT")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi images (geotiff)}
  \item{region}{name of the region (same as in the ndvi tif images).}
  \item{Ystart}{year of start of the time series.}
  \item{Yend}{year of end of the time series}
  \item{outfile}{file where the list is saved (extenstion ".txt"). By default, outfile="list.txt".}
  \item{type}{type of data. VITO_CLIP,VITO_VGT or GIMMS}
}
\details{
The format of the output file is as follows:\cr
\verb{N}\cr
\verb{path\image1.tif}\cr
\verb{path\image2.tif}\cr
\verb{...}\cr
\verb{path\imageN.tif}\cr
with \verb{N} the number of image files. \cr
File names of image are in chronological order.
It is important, for further utilisation, that \var{ndvidirectory} include the complete path to the file. 
}
\value{
Nothing return.
}
\references{
Jonsson, P. and Eklundh, L. 2004, \emph{Computers & Geosciences}, TIMESAT a program for analyzing time-series of satellite sensor data, \bold{30}, 833--845
}
\author{Romain Frelat and Bruno Gerard}
\seealso{\code{\link{ExtractFile}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{tools}
