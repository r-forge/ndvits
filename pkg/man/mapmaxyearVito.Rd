\name{mapmaxyearVito}
\alias{mapmaxyearVito}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute max per year
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mapmaxyearVito(ndvidirectory, region, year, xlim = NULL, ylim = NULL, fileout = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndvidirectory}{
%%     ~~Describe \code{ndvidirectory} here~~
}
  \item{region}{
%%     ~~Describe \code{region} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{fileout}{
%%     ~~Describe \code{fileout} here~~
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
function (ndvidirectory, region, year, xlim = NULL, ylim = NULL, 
    fileout = FALSE) 
{
    filein = paste(ndvidirectory, region, as.character(year), 
        "M01P0.tif", sep = "")
    inGrid = readpartGDAL(filein, xlim, ylim)
    tp = inGrid
    for (month in seq(1, 12, 1)) {
        if (month < 10) {
            monthstring = paste("0", as.character(month), sep = "")
        }
        else {
            monthstring = as.character(month)
        }
        for (period in seq(0, 2, 1)) {
            filein = paste(ndvidirectory, region, as.character(year), 
                "M", monthstring, "P", as.character(period), 
                ".tif", sep = "")
            inGrid = readpartGDAL(filein, xlim, ylim)
            tp$band1 = apply(cbind(tp$band1, inGrid$band1), 1, 
                maxNA2)
        }
    }
    if (fileout != FALSE) {
        writeGDAL(tp, fileout, drivername = "GTiff", type = "Byte")
    }
    return(tp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
