readpartGDAL=function(x, xlim = NULL, ylim = NULL, ...) 
{ 
require(rgdal) 
info <- GDALinfo(x) 
offs <- info[c("ll.x", "ll.y")] 
scl <- info[c("res.x", "res.y")] 
dimn <- info[c("columns", "rows")] 
xs <- seq(offs[1], by = scl[1], length = dimn[1]) + scl[1]/2 
ys <- seq(offs[2], by = scl[2], length = dimn[2]) + scl[2]/2 
xind=1:length(xs)-1
if (!is.null(xlim)) 
{ 
if (!is.numeric(xlim)) stop("xlim must be numeric") 
if (!length(xlim) == 2) stop("xlim must be of length 2") 
if (!diff(xlim) > 0) stop("xlim[1] must be less than xlim[2]") 
xind <- which(xs >= xlim[1] & xs <= xlim[2]) 
} 
yind=1:length(ys)
if (!is.null(ylim)) 
{ 
if (!is.numeric(ylim)) stop("ylim must be numeric") 
if (!length(ylim) == 2) stop("ylim must be of length 2") 
if (!diff(ylim) > 0) stop("ylim[1] must be less than ylim[2]") 
yind <- which(ys >= ylim[1] & ys <= ylim[2]) 
}
## reverse for y/x order in readGDAL 
rgdal.offset <- rev(c(min(xind), dimn[2] - max(yind))) 
rgdal.dim <- rev(c(length(xind), length(yind))) 
readGDAL(x, offset = rgdal.offset, region.dim = rgdal.dim, silent=TRUE, ...) 
}

timetoMap=function(ndvidirectory,region,year,month,period,type)
{
while(!toupper(type)%in%c("GIMMS","VITO_CLIP","VITO_VGT","TEXT","FILES"))
{
type=readline(cat("Type is not correct. Please choose between GIMMS, VITO_CLIP, VITO_VGT, TEXT and FILES. \n"))
if (type=="") return()
}
dicperiodVGT=c("01","11","21")
dicmonths=c("01","02","03","04","05","06","07","08","09","10","11","12")
filedate = c('jan15a','jan15b','feb15a','feb15b','mar15a','mar15b','apr15a','apr15b','may15a','may15b','jun15a','jun15b','jul15a','jul15b','aug15a','aug15b','sep15a','sep15b','oct15a','oct15b','nov15a','nov15b','dec15a','dec15b')
codef=c('n07-VIg','n09-VIg','n11-VIg','n14-VIg','n16-VIg','n17-VIg')
if (toupper(type)=="VITO_CLIP")
{
filein=paste(ndvidirectory,region,as.character(year),"M",dicmonths[month],"P",as.character(period-1),".tif",sep="")
}
if (toupper(type)=="VITO_VGT")
{
filein = paste(ndvidirectory,"NDV_",as.character(year),dicmonths[month],dicperiodVGT[period],"_",region,"_Extract.tif",sep="")
}
if (toupper(type)=="GIMMS")
{
if (period>2)
{ print("error gimms data are bimensual.")}
else{
files=list.files(path = paste(ndvidirectory,".",sep=""))
n=1
i=2*(month-1)+period
filein = paste(region,substr(as.character(year),3,4),filedate[i],'.',codef[n],'.tif',sep="")
#testing the different names
	while (!(filein %in%files))
	{
	if(n<7)
	{
	n=n+1
	filein = paste(region,substr(as.character(year),3,4),filedate[i],'.',codef[n],'.tif',sep="")
	}
	else
	{
	print(paste('Maps for ',as.character(year)," - ",filedate[i],' not found.',sep=""))
	return()
	}
	}
}
}
#if exist
if (file.exists(filein))
{return(filein)}
else
{
print("Error file doesn't exist !")
return(filein)
}		
}

shapelim=function(shape,shapedir,ext="shp",around=0.05)
{
if (ext=="shp")
{
#read the shapefile
inPoints=readOGR(paste(shapedir,".",sep=""),shape)
}
else
{
#read the kml file : shapedir = name.kml, shapefile= name of layer
Temp=readOGR(shapedir,shape)
if (length(grep("oint",class(Temp)))>0)
inPoints=SpatialPoints(coords=coordinates(Temp)[,1:2],proj4string=CRS(proj4string(Temp)))
if (length(grep("olygon",class(Temp)))>0)
inPoints=Temp
}
res=c()
res$xlim=bbox(inPoints)[1,]+c(-0.05,0.05)
res$ylim=bbox(inPoints)[2,]+c(-0.05,0.05)
res$proj=CRS(proj4string(inPoints))
return(res)
}

maxNA=function(x)
{
return(max(x[!is.na(x)]))
}
minNA=function(x)
{
return(min(x[!is.na(x)]))
}
meanNA=function(x)
{
return(mean(x[!is.na(x)]))
}
sdNA=function(x)
{
return(sd(x[!is.na(x)]))
}

maxNA2=function(x)
{
if (is.na(x[1])) return(x[2])
else {
if (is.na(x[2])) return(x[1])
else return(max(x))
}
}
minNA2=function(x)
{
if (is.na(x[1])) return(x[2])
else {
if (is.na(x[2])) return(x[1])
else return(min(x))
}
}
logvraineg <- function(param, obs) 
{
p <- param[1]
m1 <- param[2]
sd1 <- param[3]
m2 <- param[4]
sd2 <- param[5]
-sum(log(p * dnorm(obs, m1, sd1) + (1 - p) * dnorm(obs, m2,sd2)))
}