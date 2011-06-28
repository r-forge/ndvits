\name{ExtractGIMMS}
\alias{ExtractGIMMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extrqct GIMMS time series
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ExtractGIMMS(shapefile, shapedir, ndvidirectory, region, outfile, Ystart, Yend, ext = "shp")
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
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{Ystart}{
%%     ~~Describe \code{Ystart} here~~
}
  \item{Yend}{
%%     ~~Describe \code{Yend} here~~
}
  \item{ext}{
%%     ~~Describe \code{ext} here~~
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
function (shapefile, shapedir, ndvidirectory, region, outfile, 
    Ystart, Yend, ext = "shp") 
{
    while (!toupper(region) \%in\% c("AF", "AZ", "EA", "NA", "SA", 
        "")) {
        region = readline(cat("Region is not correct. Please choose between AF, AZ, EA, NA and SA. \n"))
    }
    while (!tolower(ext) \%in\% c("shp", "kml")) {
        ext = readline(cat("Extension is not correct. Please choose between shp and kml. \n"))
        if (ext == "") 
            return()
    }
    if (ext == "shp") {
        inPoints = readOGR(paste(shapedir, ".", sep = ""), shapefile)
    }
    else {
        inPoints = readOGR(shapedir, shapefile)
    }
    if (dim(coordinates(inPoints))[2] > 2) {
        inPoints = SpatialPointsDataFrame(coords = coordinates(inPoints)[, 
            1:2], proj4string = CRS(proj4string(inPoints)), data = as.data.frame(inPoints[names(inPoints)]))
    }
    pro = strsplit(proj4string(inPoints), "[[:punct:]]")[[1]]
    if (!(pro[grep("proj", pro) + 1] == "aea " & pro[grep("ellps", 
        pro) + 1] == "WGS84 ")) {
        inPoints = spTransform(inPoints, CRS("+proj=aea +ellps=WGS84 +lat_1=-19 +lat_2=21 +lat_0=1 +lon_0=20 +x_0=0 +y_0=0"))
    }
    filedate = c("jan15a", "jan15b", "feb15a", "feb15b", "mar15a", 
        "mar15b", "apr15a", "apr15b", "may15a", "may15b", "jun15a", 
        "jun15b", "jul15a", "jul15b", "aug15a", "aug15b", "sep15a", 
        "sep15b", "oct15a", "oct15b", "nov15a", "nov15b", "dec15a", 
        "dec15b")
    codef = c("n07-VIg", "n09-VIg", "n11-VIg", "n14-VIg", "n16-VIg", 
        "n17-VIg")
    ndvits = c()
    files = list.files(path = paste(ndvidirectory, ".", sep = ""))
    for (year in seq(Ystart, Yend, 1)) {
        cat(paste("Processing year", as.character(year), "\n"))
        for (i in seq(1, 24, 1)) {
            n = 1
            filein = paste(region, substr(as.character(year), 
                3, 4), filedate[i], ".", codef[n], ".tif", sep = "")
            while (!(filein \%in\% files)) {
                if (n < 7) {
                  n = n + 1
                  filein = paste(region, substr(as.character(year), 
                    3, 4), filedate[i], ".", codef[n], ".tif", 
                    sep = "")
                }
                else {
                  print(paste("Maps for ", as.character(year), 
                    " - ", filedate[i], " not found.", sep = ""))
                  return()
                }
            }
            inGrid = readpartGDAL(paste(ndvidirectory, filein, 
                sep = ""), bbox(inPoints)[1, ] + c(-25000, 25000), 
                bbox(inPoints)[2, ] + c(-25000, 25000))
            if (length(grep("oint", class(inPoints))) > 0) {
                ndvits = cbind(ndvits, overlay(inGrid, inPoints)$band1)
                colnames(ndvits) = c(colnames(ndvits)[-length(ndvits[1, 
                  ])], paste(as.character(year), "_", as.character(i), 
                  sep = ""))
            }
            else {
                if (length(grep("olygon", class(inPoints))) > 
                  0) {
                  ndvits = cbind(ndvits, inGrid@data[!is.na(overlay(inGrid, 
                    inPoints)), ])
                  colnames(ndvits) = c(colnames(ndvits)[-length(ndvits[1, 
                    ])], paste(as.character(year), "_", as.character(i), 
                    sep = ""))
                }
                else {
                  print(paste("The type of the ", ext, " file is unknown.", 
                    sep = ""))
                }
            }
        }
    }
    if (ext == "shp") {
        if (length(grep("oint", class(inPoints))) > 0) {
            all = data.frame(inPoints[names(inPoints)], ndvits)
        }
        if (length(grep("olygon", class(inPoints))) > 0) {
            a = overlay(inGrid, inPoints)
            xa = coordinates(inGrid)[!is.na(a), 1]
            ya = coordinates(inGrid)[!is.na(a), 2]
            na = c()
            for (i in names(inPoints)) {
                na = cbind(na, as.character(inPoints[[i]][a[!is.na(a)]]))
            }
            colnames(na) = names(inPoints)
            all = data.frame(cbind(xa, ya, na, ndvits))
        }
    }
    else {
        if (length(grep("oint", class(inPoints))) > 0) {
            all = data.frame(cbind(as.character(inPoints$Name), 
                ndvits))
        }
        if (length(grep("olygon", class(inPoints))) > 0) {
            a = overlay(inGrid, inPoints)
            xa = coordinates(inGrid)[!is.na(a), 1]
            ya = coordinates(inGrid)[!is.na(a), 2]
            name = as.character(inPoints$Name[a[!is.na(a)]])
            all = data.frame(cbind(name, ndvits))
        }
    }
    write.table(all, outfile, quote = F, row.names = T, sep = "\t")
    return(all)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
