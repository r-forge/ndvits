PeriodAnomaly=function(ndvidirectory,region,Ystart,Yend,period=36,namefile="anomaly",ext="show",xlim=NULL,ylim=NULL,obj=NULL,objdir=NULL,objext="shp",label=FALSE,pal="Spectral")
{
#creation temporary variable tp
filein = paste(ndvidirectory,region,as.character(Ystart),"M01P0.tif",sep="")
tp=readpartGDAL(filein,xlim,ylim)
#loop
#1 < period < 36
if (length(period)==1)
{
period=0:period-1
}
Mean=maplocalstatVito(ndvidirectory,region,Ystart,Yend,xlim,ylim)$mean
par(ask=T)
for (i in period)
{
#initialisation temp variable
tp$band1=rep(0,length(tp$band1))
#identification period in month and period
p=i%%3
m=trunc(i/3)+1
if (m < 10)
{
monthstring = paste("0",as.character(m),sep="")
} else 
{
monthstring = as.character(m)
}
	for (j in seq(Ystart,Yend,1))
	{
	#load the geotiff of the i period for all the year
	filein = paste(ndvidirectory,region,as.character(j),"M",monthstring,"P",as.character(p),".tif",sep="")
	inGrid = readpartGDAL(filein,xlim,ylim)
	tp$band1=tp$band1+inGrid$band1
	}
#compute the mean
tp$band1=(tp$band1/(Yend-Ystart+1))
writeGDAL(tp,paste("M",as.character(monthstring),"P",as.character(p),".tif",sep=""),drivername="GTiff", type="Byte",mvFlag=255)
tp$band1=tp$band1-Mean$band1
writeGDAL(tp,paste(namefile,"M",as.character(monthstring),"P",as.character(p),".tif",sep=""),drivername="GTiff", type="Int16",mvFlag=-32768)
#save the image
savemap(tp,obj,objdir,ext,paste(namefile,"M",as.character(monthstring),"P",as.character(p),sep=""),pal,objext,label)
}
}