\name{AnnualAnomaly}
\alias{AnnualAnomaly}
\title{
Annual Anomaly
}
\description{
The function AnnualAnomaly compute the anomaly map of the difference of the maximum of each year with their mean.
}
\usage{
AnnualAnomaly(ndvidirectory, region, Ystart, Yend, ext = "show", namefile = "anomaly", xlim = NULL, ylim = NULL, obj = NULL, objdir = NULL, objext = "shp", label = FALSE, pal = "Spectral")
}
\arguments{
  \item{ndvidirectory}{directory of the ndvi files}
  \item{region}{name of the region}
  \item{Ystart}{year of start of the analysis}
  \item{Yend}{year of end}
  \item{ext}{
%%     ~~Describe \code{ext} here~~
}
  \item{namefile}{prefixe of the name of the file. By default namefile="anomaly"
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
function(ndvidirectory,region,Ystart,Yend,ext="show",namefile="anomaly",xlim=NULL,ylim=NULL,obj=NULL,objdir=NULL,objext="shp",label=FALSE,pal="Spectral")
{
#computing the max per year
dat=c()
tab=c()
for (year in seq(Ystart, Yend, 1))
{
dat=cbind(dat,mapmaxyearVito(ndvidirectory,region,year,xlim,ylim)$band1)
colnames(dat)=c(colnames(dat)[-length(dat[1,])],as.character(year))
  }
#calculating the mean of the max
Mean=apply(dat,1,meanNA)
#computing anomalies:
tp=timetoMap(ndvidirectory,region,Ystart,1,0,xlim,ylim)
par(ask=TRUE)
for (year in seq(Ystart, Yend, 1))
{
tp$band1=dat[,as.character(year)]-Mean
writeGDAL(tp,paste(namefile,as.character(year),".tif",sep=""),drivername="GTiff", type="Int16",mvFlag=-32768)
#save the image
savemap(tp,obj,objdir,ext,paste(namefile,as.character(year),sep=""),pal,objext,label)
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
