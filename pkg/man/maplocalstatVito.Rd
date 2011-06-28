\name{maplocalstatVito}
\alias{maplocalstatVito}
\title{Local stat VITO NDVI maps}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
maplocalstatVito(ndvidirectory, region, Ystart, Yend, xlim = NULL, ylim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndvidirectory}{
%%     ~~Describe \code{ndvidirectory} here~~
}
  \item{region}{
%%     ~~Describe \code{region} here~~
}
  \item{Ystart}{
%%     ~~Describe \code{Ystart} here~~
}
  \item{Yend}{
%%     ~~Describe \code{Yend} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
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
function (ndvidirectory, region, Ystart, Yend, xlim = NULL, ylim = NULL) 
{
    filein = paste(ndvidirectory, region, as.character(Ystart), 
        "M01P0.tif", sep = "")
    inGrid = readpartGDAL(filein, xlim, ylim)
    tp = inGrid
    tp2 = inGrid
    tp3 = inGrid
    tp4 = inGrid
    mat = c()
    for (year in seq(Ystart, Yend, 1)) {
        for (month in seq(1, 12, 1)) {
            if (month < 10) {
                monthstring = paste("0", as.character(month), 
                  sep = "")
            }
            else {
                monthstring = as.character(month)
            }
            for (period in seq(0, 2, 1)) {
                filein = paste(ndvidirectory, region, as.character(year), 
                  "M", monthstring, "P", as.character(period), 
                  ".tif", sep = "")
                inGrid = readpartGDAL(filein, xlim, ylim)
                mat = cbind(mat, inGrid$band1)
            }
        }
    }
    tp$band1 = apply(mat, 1, meanNA)
    tp2$band1 = apply(mat, 1, maxNA)
    tp3$band1 = apply(mat, 1, minNA)
    tp4$band1 = apply(mat, 1, sdNA)
    writeGDAL(tp, paste(region, "mean", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Byte")
    writeGDAL(tp2, paste(region, "max", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Byte")
    writeGDAL(tp3, paste(region, "min", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Byte")
    writeGDAL(tp4, paste(region, "sd", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Byte")
    res = c()
    res$mean = tp
    res$max = tp2
    res$min = tp3
    res$sd = tp4
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
