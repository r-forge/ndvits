\name{OverlayTS}
\alias{OverlayTS}
\title{ Overlay Time series }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OverlayTS(TS, Ystart, period = 36, title = "NDVI Time Series", outfile = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS}{
%%     ~~Describe \code{TS} here~~
}
  \item{Ystart}{
%%     ~~Describe \code{Ystart} here~~
}
  \item{period}{
%%     ~~Describe \code{period} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
function (TS, Ystart, period = 36, title = "NDVI Time Series", 
    outfile = FALSE) 
{
    if (outfile != FALSE) {
        pdf(outfile)
    }
    liM = max(TS)
    lim = min(TS) - 0.1
    if (dim(as.data.frame(TS))[2] > 1) {
        plot(ts(TS[1, ], start = Ystart, freq = period), main = title, 
            ylab = "NDVI", ylim = c(lim, liM), col = rainbow(1)[1])
        for (i in 2:length(TS[, 1])) lines(ts(TS[i, ], start = Ystart, 
            freq = period), col = rainbow(length(TS[, 1]))[i])
        legend("bottomright", legend = rownames(TS), lwd = 2, 
            col = rainbow(length(TS[, 1])))
    }
    else {
        print("Only one time series : no overlay possible.")
    }
    if (outfile != FALSE) {
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
