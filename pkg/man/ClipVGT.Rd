\name{ClipVGT}
\alias{ClipVGT}
\title{
Clip VGT data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ClipVGT(ndvidirectory, region, xlim, ylim, Ystart, Yend, outdirectory, regionout)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ndvidirectory}{
%%     ~~Describe \code{ndvidirectory} here~~
}
  \item{region}{
%%     ~~Describe \code{region} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{Ystart}{
%%     ~~Describe \code{Ystart} here~~
}
  \item{Yend}{
%%     ~~Describe \code{Yend} here~~
}
  \item{outdirectory}{
%%     ~~Describe \code{outdirectory} here~~
}
  \item{regionout}{
%%     ~~Describe \code{regionout} here~~
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
function (ndvidirectory, region, xlim, ylim, Ystart, Yend, outdirectory, 
    regionout) 
{
    dicperiod = c("01", "11", "21")
    dicmonths = c("01", "02", "03", "04", "05", "06", "07", "08", 
        "09", "10", "11", "12")
    if (!file.exists(substr(outdirectory, start = 1, stop = nchar(outdirectory) - 
        1))) {
        dir.create(outdirectory)
    }
    for (year in seq(Ystart, Yend, 1)) {
        print(paste("Clipping year", as.character(year)))
        for (month in seq(1, 12, 1)) {
            for (period in seq(1, 3, 1)) {
                filein = paste(ndvidirectory, "NDV_", as.character(year), 
                  dicmonths[month], dicperiod[period], "_", region, 
                  "_Extract.tif", sep = "")
                inGrid = readpartGDAL(filein, xlim, ylim)
                fileout = paste(outdirectory, regionout, as.character(year), 
                  "M", dicmonths[month], "P", as.character(period - 
                    1), ".tif", sep = "")
                writeGDAL(inGrid, fileout, drivername = "GTiff", 
                  type = "Byte", mvFlag = 255)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
