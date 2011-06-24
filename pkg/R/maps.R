maplocalstatVito=function(ndvidirectory,region,Ystart,Yend,xlim=NULL,ylim=NULL)
{
filein = paste(ndvidirectory,region,as.character(Ystart),"M01P0.tif",sep="")
inGrid=readpartGDAL(filein,xlim,ylim)
tp=inGrid
tp2=inGrid
tp3=inGrid
tp4=inGrid
mat=c()
for (year in seq(Ystart, Yend, 1))
{
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
			inGrid = readpartGDAL(filein,xlim,ylim)
			mat=cbind(mat,inGrid$band1)
		}
	}
}
tp$band1=apply(mat,1,meanNA)
tp2$band1=apply(mat,1,maxNA)
tp3$band1=apply(mat,1,minNA)
tp4$band1=apply(mat,1,sdNA)
#saving the time series
writeGDAL(tp,paste(region,"mean",substr(as.character(Ystart),3,4),substr(as.character(Yend),3,4),".tif",sep=""),drivername="GTiff", type="Byte")
writeGDAL(tp2,paste(region,"max",substr(as.character(Ystart),3,4),substr(as.character(Yend),3,4),".tif",sep=""),drivername="GTiff", type="Byte")
writeGDAL(tp3,paste(region,"min",substr(as.character(Ystart),3,4),substr(as.character(Yend),3,4),".tif",sep=""),drivername="GTiff", type="Byte")
writeGDAL(tp4,paste(region,"sd",substr(as.character(Ystart),3,4),substr(as.character(Yend),3,4),".tif",sep=""),drivername="GTiff", type="Byte")
res=c()
res$mean=tp
res$max=tp2
res$min=tp3
res$sd=tp4
return(res)
}

mapmaxyearVito=function(ndvidirectory,region,year,xlim=NULL,ylim=NULL,fileout=FALSE)
{
filein = paste(ndvidirectory,region,as.character(year),"M01P0.tif",sep="")
inGrid=readpartGDAL(filein,xlim,ylim)
tp=inGrid
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
		inGrid = readpartGDAL(filein,xlim,ylim)
		tp$band1=apply(cbind(tp$band1,inGrid$band1),1,maxNA2)
	}
}
if(fileout!=FALSE)
{
writeGDAL(tp,fileout,drivername="GTiff", type="Byte")
}
return(tp)
}

mapdiff=function(mapin,mapref,mapsd=FALSE,fileout="diff.tif")
{
if(as.numeric(bbox(mapin))!=as.numeric(bbox(mapref))) stop("the two maps must be the same size") 
res=mapref
res$band1=mapin$band1-mapref$band1
writeGDAL(res,fileout,drivername="GTiff", type="Int16",mvFlag=-32768)
return(res)
}

