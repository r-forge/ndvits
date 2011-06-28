\name{PeriodAnomaly}
\alias{PeriodAnomaly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Period Anomaly map
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PeriodAnomaly(ndvidirectory, region, Ystart, Yend, period = 36, namefile = "anomaly", ext = "show", xlim = NULL, ylim = NULL, obj = NULL, objdir = NULL, objext = "shp", label = FALSE, pal = "Spectral")
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
  \item{period}{
%%     ~~Describe \code{period} here~~
}
  \item{namefile}{
%%     ~~Describe \code{namefile} here~~
}
  \item{ext}{
%%     ~~Describe \code{ext} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{objdir}{
%%     ~~Describe \code{objdir} here~~
}
  \item{objext}{
%%     ~~Describe \code{objext} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{pal}{
%%     ~~Describe \code{pal} here~~
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
function (ndvidirectory, region, Ystart, Yend, period = 36, namefile = "anomaly", 
    ext = "show", xlim = NULL, ylim = NULL, obj = NULL, objdir = NULL, 
    objext = "shp", label = FALSE, pal = "Spectral") 
{
    filein = paste(ndvidirectory, region, as.character(Ystart), 
        "M01P0.tif", sep = "")
    tp = readpartGDAL(filein, xlim, ylim)
    if (length(period) == 1) {
        period = 0:period - 1
    }
    Mean = maplocalstatVito(ndvidirectory, region, Ystart, Yend, 
        xlim, ylim)$mean
    par(ask = T)
    for (i in period) {
        tp$band1 = rep(0, length(tp$band1))
        p = i\%\%3
        m = trunc(i/3) + 1
        if (m < 10) {
            monthstring = paste("0", as.character(m), sep = "")
        }
        else {
            monthstring = as.character(m)
        }
        for (j in seq(Ystart, Yend, 1)) {
            filein = paste(ndvidirectory, region, as.character(j), 
                "M", monthstring, "P", as.character(p), ".tif", 
                sep = "")
            inGrid = readpartGDAL(filein, xlim, ylim)
            tp$band1 = tp$band1 + inGrid$band1
        }
        tp$band1 = (tp$band1/(Yend - Ystart + 1))
        writeGDAL(tp, paste("M", as.character(monthstring), "P", 
            as.character(p), ".tif", sep = ""), drivername = "GTiff", 
            type = "Byte", mvFlag = 255)
        tp$band1 = tp$band1 - Mean$band1
        writeGDAL(tp, paste(namefile, "M", as.character(monthstring), 
            "P", as.character(p), ".tif", sep = ""), drivername = "GTiff", 
            type = "Int16", mvFlag = -32768)
        savemap(tp, obj, objdir, ext, paste(namefile, "M", as.character(monthstring), 
            "P", as.character(p), sep = ""), pal, objext, label)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
