\name{mapdiff}
\alias{mapdiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute difference of 2 maps
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mapdiff(mapin, mapref, mapsd = FALSE, fileout = "diff.tif")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapin}{
%%     ~~Describe \code{mapin} here~~
}
  \item{mapref}{
%%     ~~Describe \code{mapref} here~~
}
  \item{mapsd}{
%%     ~~Describe \code{mapsd} here~~
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
function (mapin, mapref, mapsd = FALSE, fileout = "diff.tif") 
{
    if (as.numeric(bbox(mapin)) != as.numeric(bbox(mapref))) 
        stop("the two maps must be the same size")
    res = mapref
    res$band1 = mapin$band1 - mapref$band1
    writeGDAL(res, fileout, drivername = "GTiff", type = "Int16", 
        mvFlag = -32768)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
