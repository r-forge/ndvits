\name{pointtobuffer}
\alias{pointtobuffer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
point to buffer
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pointtobuffer(shapefile, shapedir, ndvidirectory, region, Ystart, nameshape = "buffer", dirshape = ".", rad = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shapefile}{
%%     ~~Describe \code{shapefile} here~~
}
  \item{shapedir}{
%%     ~~Describe \code{shapedir} here~~
}
  \item{ndvidirectory}{
%%     ~~Describe \code{ndvidirectory} here~~
}
  \item{region}{
%%     ~~Describe \code{region} here~~
}
  \item{Ystart}{
%%     ~~Describe \code{Ystart} here~~
}
  \item{nameshape}{
%%     ~~Describe \code{nameshape} here~~
}
  \item{dirshape}{
%%     ~~Describe \code{dirshape} here~~
}
  \item{rad}{
%%     ~~Describe \code{rad} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (shapefile, shapedir, ndvidirectory, region, Ystart, 
    nameshape = "buffer", dirshape = ".", rad = 1) 
{
    inPoints = readOGR(shapedir, shapefile)
    if (dim(coordinates(inPoints))[2] > 2) {
        inPoints = SpatialPointsDataFrame(coords = coordinates(inPoints)[, 
            1:2], proj4string = CRS(proj4string(inPoints)), data = as.data.frame(inPoints[names(inPoints)]))
    }
    filein = timetoMap(ndvidirectory, region, Ystart, 8, 2, type = "VITO_VGT")
    inGrid = readpartGDAL(filein, bbox(inPoints)[1, ] + c(-0.05, 
        0.05), bbox(inPoints)[2, ] + c(-0.05, 0.05))
    coord = c()
    data = c()
    for (i in 1:length(coordinates(inPoints)[, 1])) {
        for (x in -rad:rad) {
            for (y in -rad:rad) {
                coord = rbind(coord, coordinates(inPoints)[i, 
                  ] + c(x * gridparameters(inGrid)[1, 2], y * 
                  gridparameters(inGrid)[2, 2]))
                data = rbind(data, as.data.frame(inPoints[names(inPoints)])[i, 
                  ])
            }
        }
    }
    names(data) = substr(names(data), 1, 10)
    res = SpatialPointsDataFrame(coords = coord, proj4string = CRS(proj4string(inPoints)), 
        data = as.data.frame(data))
    writeOGR(res, dirshape, layer = nameshape, driver = "ESRI Shapefile")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
