TimeSeriesAnalysis=function(shapefile,shapedir,ndvidirectory,region,Ystart,Yend,outfile="TS.txt",outfile2="TS.pdf",outfile3=FALSE,ext="shp",fct="mean",SGfilter=TRUE,nSG="5,5", DSG=0,title="NDVI time series",type="VITO_CLIP")
{
#check the extension of the shape/kml files
while(!tolower(ext)%in%c("shp","kml"))
{
ext=readline(cat("Extension is not correct. Please choose between shp and kml. \n"))
if (ext=="") return()
}
#seeking information on the points/polygons
# select or ask the grouping factor of the points/polygone
if (tolower(ext)=="shp")
{
#read the shapefile
inPoints=readOGR(paste(shapedir,".",sep=""),shapefile)
info=names(inPoints)
#selecting the grouping factor
if (length(info)==1)
{
fac=as.factor(inPoints[[info]])
}else{
f=""
while(!f%in%info)
{
f=readline(cat(paste("choose between one of the grouping factor available : \n",list(as.character(info)),"\n : ",sep="")))
}
fac=as.factor(inPoints[[f]])
}}
else
{
inPoints=readOGR(shapedir,shapefile)
fac=as.factor(inPoints$Name)
}
while(!toupper(type)%in%c("GIMMS","VITO_CLIP","VITO_VGT","TEXT","FILES"))
{
type=readline(cat("Type is not correct. Please choose between GIMMS, VITO_CLIP, VITO_VGT, TEXT and FILES. \n"))
if (type=="") return()
}
#extracting time series
if (toupper(type)=="VITO_CLIP")
{
TS=ExtractVito(shapefile,shapedir,ndvidirectory,region,outfile,Ystart,Yend,ext)
period=36
max=255
}
if (toupper(type)=="VITO_VGT")
{
TS=ExtractVGT(shapefile,shapedir,ndvidirectory,region,outfile,Ystart,Yend,ext)
period=36
max=255
}
if (toupper(type)=="GIMMS")
{
TS=ExtractGIMMS(shapefile,shapedir,ndvidirectory,region,outfile,Ystart,Yend,ext)
period=24
max=10000
}
if (toupper(type)=="TEXT")
{
period=as.numeric(readline(cat("how many observation per year ?\n")))
max=as.numeric(readline(cat("value maximum ?\n")))
TS=read.table(ndvidirectory, header=TRUE,sep="\t",row.names=1)
}
if (toupper(type)=="FILES")
{
period=readline(cat("how many observation per year ?\n"))
TS=ExtractFiles(shapefile,shapedir,ndvidirectory,files,outfile,Ystart,Yend,ext)
}
#computing normalized ndvi (for vito, max=255)
ndvi=normNDVI(TS,max)
#Seasonal Decomposition
TS2=SeasonalDecompositionperArea(ndvi,fac,outfile2,Ystart,period,fct,SGfilter,nSG,DSG)
#Plotting the time series together
PlotTS(TS2,outfile3,Ystart,period,title)
return(TS2)
}


#ExtractVitoShp is a function to extract automatically
#NDVI time series from VITO data of points in shapefile
#shapefile : name of the shapefile (multipoints)
#shapedir : direction of the folder containing the shapefile
#ndvi directory : directory of the ndvi images (geotiff)
#region : name of the region (like in the ndvi tiff images)
#outfile : file where the time serie is saved
#Ystart - Yend : year of start/end of the time series available
ExtractVito=function(shapefile,shapedir,ndvidirectory,region,outfile="TS.txt",Ystart,Yend, ext="shp")
{
while(!tolower(ext)%in%c("shp","kml"))
{
ext=readline(cat("Extension is not correct. Please choose between shp and kml. \n"))
if (ext=="") return()
}
if (tolower(ext)=="shp")
{
#read the shapefile
inPoints=readOGR(paste(shapedir,".",sep=""),shapefile)
}
else
{
#read the kml file : shapedir = name.kml, shapefile= name of layer
inPoints=readOGR(shapedir,shapefile)
name=inPoints$Name
}
#keeping only x and y coordinates
if(dim(coordinates(inPoints))[2]>2)
{
inPoints=SpatialPointsDataFrame(coords=coordinates(inPoints)[,1:2],proj4string=CRS(proj4string(inPoints)),data=as.data.frame(inPoints[names(inPoints)]))
}
#projection of the points on the image coordinate systems 
pro=strsplit(proj4string(inPoints),"[[:punct:]]")[[1]]
if(!(pro[grep("proj",pro)+1]=="longlat "&pro[grep("ellps",pro)+1]=="WGS84 "&pro[grep("datum",pro)+1]=="WGS84 "))
{
inPoints=spTransform(inPoints,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
}
#main loop : extracting time series
ndvits=c()
for (year in seq(Ystart, Yend, 1))
{
	cat(paste("Processing year",as.character(year),"\n"))
	for (month in seq(1, 12, 1))
	{
		if (month < 10)
		{
		monthstring = paste("0",as.character(month),sep="")
		}
		else 
		{
		monthstring = as.character(month)
		}	
		for (period in seq(0, 2, 1))
		{
		#load the geotiff
		filein = paste(ndvidirectory,region,as.character(year),"M",monthstring,"P",as.character(period),".tif",sep="")
		inGrid = readpartGDAL(filein,bbox(inPoints)[1,]+c(-0.05,0.05),bbox(inPoints)[2,]+c(-0.05,0.05))
		#extract ndvi values of the points
		if (length(grep("oint",class(inPoints)))>0)
		{
		ndvits=cbind(ndvits,overlay(inGrid,inPoints)$band1)
		colnames(ndvits)=c(colnames(ndvits)[-length(ndvits[1,])],paste(as.character(year),monthstring,as.character(period),sep=""))
		} else {
		if (length(grep("olygon",class(inPoints)))>0)
		{
		ndvits=cbind(ndvits,inGrid@data[!is.na(overlay(inGrid,inPoints)),])
		colnames(ndvits)=c(colnames(ndvits)[-length(ndvits[1,])],paste(as.character(year),monthstring,as.character(period),sep=""))
		} else {
		print("The type of ths shp/kml file is unknown.")}
		}
		}
	}
}
#saving the time series
if (ext=="shp")
{
if (length(grep("oint",class(inPoints)))>0) {
all=data.frame(inPoints[names(inPoints)],ndvits) }
if (length(grep("olygon",class(inPoints)))>0) {
a=overlay(inGrid,inPoints)
#coordinates of points in polygone
xa=coordinates(inGrid)[!is.na(a),1]
ya=coordinates(inGrid)[!is.na(a),2]
#names of points in polygone
na=c()
for (i in names(inPoints))
{na=cbind(na,as.character(inPoints[[i]][a[!is.na(a)]]))}
colnames(na)=names(inPoints)
all=data.frame(cbind(xa,ya,na,ndvits))
}
}
else
{
if (length(grep("oint",class(inPoints)))>0) {
all=data.frame(cbind(as.character(inPoints$Name),ndvits))}
if (length(grep("olygon",class(inPoints)))>0) {
a=overlay(inGrid,inPoints)
#coordinates of points in polygone
xa=coordinates(inGrid)[!is.na(a),1]
ya=coordinates(inGrid)[!is.na(a),2]
#names of points in polygone
name=as.character(inPoints$Name[a[!is.na(a)]])
all=data.frame(cbind(name,ndvits))
}
}
write.table(all,outfile,quote=F,row.names=T,sep="\t")
return(all)
}

ExtractVGT=function(shapefile,shapedir,ndvidirectory,region,outfile="TS.txt",Ystart,Yend, ext="shp")
{
while(!tolower(ext)%in%c("shp","kml"))
{
ext=readline(cat("Extension is not correct. Please choose between shp and kml. \n"))
if (ext=="") return()
}
if (ext=="shp")
{
#read the shapefile
inPoints=readOGR(paste(shapedir,".",sep=""),shapefile)
}
else
{
#read the kml file : shapedir = name.kml, shapefile= name of layer
inPoints=readOGR(shapedir,shapefile)
}
#keeping only x and y coordinates
if(dim(coordinates(inPoints))[2]>2)
{
inPoints=SpatialPointsDataFrame(coords=coordinates(inPoints)[,1:2],proj4string=CRS(proj4string(inPoints)),data=as.data.frame(inPoints[names(inPoints)]))
}
#projection of the points on the image coordinate systems 
pro=strsplit(proj4string(inPoints),"[[:punct:]]")[[1]]
if(!(pro[grep("proj",pro)+1]=="longlat "&pro[grep("ellps",pro)+1]=="WGS84 "&pro[grep("datum",pro)+1]=="WGS84 "))
{
inPoints=spTransform(inPoints,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
}
ndvits=c()
dicperiod=c("01","11","21")
dicmonths=c("01","02","03","04","05","06","07","08","09","10","11","12")
#main loop : extracting time series
for (year in seq(Ystart, Yend, 1))
{
	cat(paste("Processing year",as.character(year),"\n"))
	for (month in seq(1, 12, 1))
	{
		for (period in seq(1, 3, 1))
		{
		#load the geotiff
		filein = paste(ndvidirectory,"NDV_",as.character(year),dicmonths[month],dicperiod[period],"_",region,"_Extract.tif",sep="")
		inGrid = readpartGDAL(filein,bbox(inPoints)[1,]+c(-0.05,0.05),bbox(inPoints)[2,]+c(-0.05,0.05))
		#extract ndvi values of the points
		if (length(grep("oint",class(inPoints)))>0)
		{
		ndvits=cbind(ndvits,overlay(inGrid,inPoints)$band1)
		colnames(ndvits)=c(colnames(ndvits)[-length(ndvits[1,])],paste(as.character(year),dicmonths[month],as.character(period),sep=""))
		} else {
		if (length(grep("olygon",class(inPoints)))>0)
		{
		ndvits=cbind(ndvits,inGrid@data[!is.na(overlay(inGrid,inPoints)),])
		colnames(ndvits)=c(colnames(ndvits)[-length(ndvits[1,])],paste(as.character(year),dicmonths[month],as.character(period),sep=""))
		} else {
		print("The type of ths shp/kml file is unknown.")}
		}
		}
	}
}
#saving the time series
if (ext=="shp")
{
if (length(grep("oint",class(inPoints)))>0) {
all=data.frame(inPoints[names(inPoints)],ndvits) }
if (length(grep("olygon",class(inPoints)))>0) {
a=overlay(inGrid,inPoints)
#coordinates of points in polygone
xa=coordinates(inGrid)[!is.na(a),1]
ya=coordinates(inGrid)[!is.na(a),2]
#names of points in polygone
na=c()
for (i in names(inPoints))
{na=cbind(na,as.character(inPoints[[i]][a[!is.na(a)]]))}
colnames(na)=names(inPoints)
all=data.frame(cbind(xa,ya,na,ndvits))
}
}
else
{
if (length(grep("oint",class(inPoints)))>0) {
all=data.frame(cbind(as.character(inPoints$Name),ndvits))}
if (length(grep("olygon",class(inPoints)))>0) {
a=overlay(inGrid,inPoints)
#coordinates of points in polygone
xa=coordinates(inGrid)[!is.na(a),1]
ya=coordinates(inGrid)[!is.na(a),2]
#names of points in polygone
name=as.character(inPoints$Name[a[!is.na(a)]])
all=data.frame(cbind(name,ndvits))
}
}
write.table(all,outfile,quote=F,row.names=T,sep="\t")
return(all)
}

#ExtractGIMMSShp is a function to extract automatically
#NDVI time series from GIMMS data of points in shapefile
ExtractGIMMS=function(shapefile,shapedir,ndvidirectory,region,outfile,Ystart,Yend,ext="shp")
{
while(!toupper(region)%in%c("AF","AZ","EA","NA","SA",""))
{
region=readline(cat("Region is not correct. Please choose between AF, AZ, EA, NA and SA. \n"))
}
while(!tolower(ext)%in%c("shp","kml"))
{
ext=readline(cat("Extension is not correct. Please choose between shp and kml. \n"))
if (ext=="") return()
}
if (ext=="shp")
{
#read the shapefile
inPoints=readOGR(paste(shapedir,".",sep=""),shapefile)
}
else
{
#read the kml file : shapedir = name.kml, shapefile= name of layer
inPoints=readOGR(shapedir,shapefile)
}
#keeping only x and y coordinates
if(dim(coordinates(inPoints))[2]>2)
{
inPoints=SpatialPointsDataFrame(coords=coordinates(inPoints)[,1:2],proj4string=CRS(proj4string(inPoints)),data=as.data.frame(inPoints[names(inPoints)]))
}
#projection of the points on the image coordinate systems 
pro=strsplit(proj4string(inPoints),"[[:punct:]]")[[1]]
if(!(pro[grep("proj",pro)+1]=="aea "&pro[grep("ellps",pro)+1]=="WGS84 "))
{
inPoints=spTransform(inPoints,CRS("+proj=aea +ellps=WGS84 +lat_1=-19 +lat_2=21 +lat_0=1 +lon_0=20 +x_0=0 +y_0=0"))
}
filedate = c('jan15a','jan15b','feb15a','feb15b','mar15a','mar15b','apr15a','apr15b','may15a','may15b','jun15a','jun15b','jul15a','jul15b','aug15a','aug15b','sep15a','sep15b','oct15a','oct15b','nov15a','nov15b','dec15a','dec15b')
codef=c('n07-VIg','n09-VIg','n11-VIg','n14-VIg','n16-VIg','n17-VIg')
ndvits=c()
#making a list of all files in the ndvidirectory
files=list.files(path = paste(ndvidirectory,".",sep=""))
#main loop : extracting time series
for (year in seq(Ystart, Yend, 1))
{
	cat(paste("Processing year",as.character(year),"\n"))
	for (i in seq(1, 24, 1))
	{	
		n=1
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
		inGrid = readpartGDAL(paste(ndvidirectory,filein,sep=""),bbox(inPoints)[1,]+c(-25000,25000),bbox(inPoints)[2,]+c(-25000,25000))
		#extract ndvi values of the points
		if (length(grep("oint",class(inPoints)))>0)
		{
		ndvits=cbind(ndvits,overlay(inGrid,inPoints)$band1)
		colnames(ndvits)=c(colnames(ndvits)[-length(ndvits[1,])],paste(as.character(year),"_",as.character(i),sep=""))
		} else {
		#extract ndvi values of the polygons
		if (length(grep("olygon",class(inPoints)))>0)
		{
		ndvits=cbind(ndvits,inGrid@data[!is.na(overlay(inGrid,inPoints)),])
		colnames(ndvits)=c(colnames(ndvits)[-length(ndvits[1,])],paste(as.character(year),"_",as.character(i),sep=""))
		} else {
		print(paste("The type of the ",ext," file is unknown.",sep=""))}
		}
	}
}
#saving the time serie
if (ext=="shp")
{
if (length(grep("oint",class(inPoints)))>0) {
all=data.frame(inPoints[names(inPoints)],ndvits) }
if (length(grep("olygon",class(inPoints)))>0) {
a=overlay(inGrid,inPoints)
#coordinates of points in polygone
xa=coordinates(inGrid)[!is.na(a),1]
ya=coordinates(inGrid)[!is.na(a),2]
#names of points in polygone
na=c()
for (i in names(inPoints))
{na=cbind(na,as.character(inPoints[[i]][a[!is.na(a)]]))}
colnames(na)=names(inPoints)
all=data.frame(cbind(xa,ya,na,ndvits))
}
}
else
{
if (length(grep("oint",class(inPoints)))>0) {
all=data.frame(cbind(as.character(inPoints$Name),ndvits))}
if (length(grep("olygon",class(inPoints)))>0) {
a=overlay(inGrid,inPoints)
#coordinates of points in polygone
xa=coordinates(inGrid)[!is.na(a),1]
ya=coordinates(inGrid)[!is.na(a),2]
#names of points in polygone
name=as.character(inPoints$Name[a[!is.na(a)]])
all=data.frame(cbind(name,ndvits))
}
}
write.table(all,outfile,quote=F,row.names=T,sep="\t")
return(all)
}

#Compute the NDVI values between 0 and 1  
normNDVI=function(TS,maxNDVI)
{
#takes only the col containing "X" : the ndvi data
ndvi=TS[grep("X",names(TS))]
#NDVI beetween -1 and 1
for (i in 1:length(ndvi))
{
ndvi[,i]=as.numeric(as.character(ndvi[,i]))/maxNDVI
}
return(ndvi)
}

#Make a Seasonal Decomposition of Time Series by Loess per group of pixels
#TS : a matrix containing only NDVI data, one row per pixel
#area : factor which group pixels of the same area together
#fileout : pdf file where graph will be saved
#Ystart : start year of the ndvi data
#period : number of observation per year
#fct : function applied to group points from the same area (mean, max, ...)
#     by default mean
#SGfilter : application of Savitsky golay filter on time series
#     by default TRUE
#nSG, DSG : parameters of the SG filter (n : windows, D : degree polynome)
#     by default nSG="5,5" (windows 5,5) D=0
SeasonalDecompositionperArea=function(TS,area,fileout=FALSE,Ystart,period=36,fct="mean",SGfilter=TRUE,nSG="5,5", DSG=0)
{
if (! require(RTisean))
	{ return()}
while(!tolower(fct)%in%c("mean","max","min"))
{
fct=readline(cat("Function is not correct. Please choose between mean, max and min."))
if (fct=="") return()
}
finalndvi=c()
#make sure that area is a factor
area=as.factor(area)
#open the pdf file
if (fileout != FALSE)
{
pdf(fileout)
}
else
{
par(ask=TRUE)
}
#loop on the different area
for (i in unique(area))
{
par(oma = c(0, 0, 3, 0))
#subset of the ndvi time serie of the area
ndviLoc=TS[area==i,]
#apply the fct to group points together
ndviM=apply(ndviLoc,2,fct) 
#plot the NDVI time series
plot(ts(ndviM,start=Ystart,freq=period),ylim=c(0,1),type="l", xlab="time", ylab="NDVI",main=paste("NDVI Time Serie -",fct,"over",length(ndviLoc[,1]),"points"))
mtext(side = 3, text = paste("Spot Vegetation Data -", i), outer = TRUE,cex=1.6)
if (SGfilter)
{
filtndvi=sav_gol(ndviM, n =nSG,D =DSG)
ndviMSG=ts(filtndvi[,1],start=Ystart,freq=period)
lines(ndviMSG,col="red")
if (min(ndviMSG)>0.2 | max(ndviMSG)>0.85)
{
legend("bottomright",legend=c(paste(fct,"NDVI",sep=" "), "SG filter"), col=c("black","red"),lwd=2)
}
else
{
legend("topright",legend=c(paste(fct,"NDVI",sep=" "), "SG filter"), col=c("black","red"),lwd=2)
}
filt="SG Filtered"
}
else
{
filt=""
ndviMSG=ts(ndviM,start=Ystart,freq=period)
}
title = c(i, paste(fct,filt,'NDVI time series'))
plot(stl(ndviMSG,s.window="periodic"),main= title)
finalndvi=rbind(finalndvi,ndviMSG)
rownames(finalndvi)=c(rownames(finalndvi)[-length(finalndvi[,1])],i)
}
if (fileout != FALSE)
{
dev.off()
}
colnames(finalndvi)=as.character(round(time(ndviMSG),2))
return(finalndvi)
}
