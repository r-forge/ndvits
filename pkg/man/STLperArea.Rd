\name{STLperArea}
\alias{STLperArea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Seasonal Decomposition per Area}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
STLperArea(TS, area, fileout = FALSE, Ystart, period = 36, fct = "mean", SGfilter = TRUE, nSG = "5,5", DSG = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TS}{
%%     ~~Describe \code{TS} here~~
}
  \item{area}{
%%     ~~Describe \code{area} here~~
}
  \item{fileout}{
%%     ~~Describe \code{fileout} here~~
}
  \item{Ystart}{
%%     ~~Describe \code{Ystart} here~~
}
  \item{period}{
%%     ~~Describe \code{period} here~~
}
  \item{fct}{
%%     ~~Describe \code{fct} here~~
}
  \item{SGfilter}{
%%     ~~Describe \code{SGfilter} here~~
}
  \item{nSG}{
%%     ~~Describe \code{nSG} here~~
}
  \item{DSG}{
%%     ~~Describe \code{DSG} here~~
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
function (TS, area, fileout = FALSE, Ystart, period = 36, fct = "mean", 
    SGfilter = TRUE, nSG = "5,5", DSG = 0) 
{
    if (!require(RTisean)) {
        return()
    }
    while (!tolower(fct) \%in\% c("mean", "max", "min")) {
        fct = readline(cat("Function is not correct. Please choose between mean, max and min."))
        if (fct == "") 
            return()
    }
    finalndvi = c()
    area = as.factor(area)
    if (fileout != FALSE) {
        pdf(fileout)
    }
    else {
        par(ask = TRUE)
    }
    for (i in unique(area)) {
        par(oma = c(0, 0, 3, 0))
        ndviLoc = TS[area == i, ]
        ndviM = apply(ndviLoc, 2, fct)
        plot(ts(ndviM, start = Ystart, freq = period), ylim = c(0, 
            1), type = "l", xlab = "time", ylab = "NDVI", main = paste("NDVI Time Serie -", 
            fct, "over", length(ndviLoc[, 1]), "points"))
        mtext(side = 3, text = paste("Spot Vegetation Data -", 
            i), outer = TRUE, cex = 1.6)
        if (SGfilter) {
            filtndvi = sav_gol(ndviM, n = nSG, D = DSG)
            ndviMSG = ts(filtndvi[, 1], start = Ystart, freq = period)
            lines(ndviMSG, col = "red")
            if (min(ndviMSG) > 0.2 | max(ndviMSG) > 0.85) {
                legend("bottomright", legend = c(paste(fct, "NDVI", 
                  sep = " "), "SG filter"), col = c("black", 
                  "red"), lwd = 2)
            }
            else {
                legend("topright", legend = c(paste(fct, "NDVI", 
                  sep = " "), "SG filter"), col = c("black", 
                  "red"), lwd = 2)
            }
            filt = "SG Filtered"
        }
        else {
            filt = ""
            ndviMSG = ts(ndviM, start = Ystart, freq = period)
        }
        title = c(i, paste(fct, filt, "NDVI time series"))
        plot(stl(ndviMSG, s.window = "periodic"), main = title)
        finalndvi = rbind(finalndvi, ndviMSG)
        rownames(finalndvi) = c(rownames(finalndvi)[-length(finalndvi[, 
            1])], i)
    }
    if (fileout != FALSE) {
        dev.off()
    }
    colnames(finalndvi) = as.character(round(time(ndviMSG), 2))
    return(finalndvi)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
