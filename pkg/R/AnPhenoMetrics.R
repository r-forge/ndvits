"AnPhenoMetrics"=function(TS,fileout,outgraph,Ystart,period,SOSth=0.5,EOSth=0.5)
{
#Extracting annual metrix
#TS : time serie or matrix of time serie containing only NDVI data     
# can be a single ts or multiple
#fileout : txt file where metrics are saved
#outgraph : pdf file where graph will be saved
#Ystart : start year of the ndvi data
#period : number of observation per year
#SOSth / EOSth : threshold for detection start/end of the season
#      by default SOSth=EOSth=0.5
if (outgraph != FALSE)
{pdf(outgraph)}
else 
{par(ask=TRUE)}
liM=max(TS)
lim=min(TS)
for (j in 1:length(TS[,1]))
{
res=c()
plot(ts(TS[j,],start=Ystart,freq=period),ylim=c(lim,liM),type="l", xlab="time", ylab="NDVI",main=paste(rownames(TS)[j],"NDVI Time Series"))
#annual decomposition
year=c()
n=1
while((n+period-1)<length(TS[1,]))
{
	year=rbind(year,TS[j,n:(n+period-1)])
	n=n+period
}
rownames(year)=Ystart:(Ystart+length(year[,1])-1)
meanC=apply(year,2,mean)
#plot the time series of different years in the same graph
plot(meanC,ylim=c(0,liM),type="l", xlab="time", ylab="NDVI",xaxt="n",main=paste(rownames(TS)[j],"NDVI Annual Time Series"),lwd=3)
axis(side=1,at=seq(1,period,period/12),labels=month.abb)
for (i in 1:length(year[,1]))
{
lines(year[i,],col=rainbow(length(year[,1]))[i])
}
legend("bottomright",legend=c("mean", "year"), lwd=c(3,1),col=c("black","red"))
#Detecting number of season
meanM=c() #dates of maximums of the year
meanm=c() #dates of minimums of the year
meanC2=c(meanC[(length(meanC)-2):length(meanC)],meanC,meanC[1:3])
for (k in 1:length(meanC))
{
if (meanC[k]==max(meanC2[(k):(k+6)]) & meanC[k]>mean(meanC))
meanM=c(meanM,k)
if (meanC[k]==min(meanC2[(k):(k+6)]) & meanC[k]<mean(meanC))
meanm=c(meanm,k)
}
#useless but just to be sure :
meanM=sort(meanM)
meanm=sort(meanm)
#max is always followed by min and vice versa
tmp=sort(c(meanM,meanm))
for (k in 1:(length(tmp)-1))
{
if (tmp[k]%in%meanm & tmp[k+1]%in%meanm)
{
meanm=meanm[-(k-1+which.max(c(meanC[tmp[k]],meanC[tmp[k+1]])))]
}
if (tmp[k]%in%meanM & tmp[k+1]%in%meanM)
meanM=meanM[-(k-1+which.min(c(meanC[tmp[k]],meanC[tmp[k+1]])))]
}
if(length(meanM)!=length(meanm))
{
print("Not same number of minimums and maximums, results may be wrong!")
}
abline(v=c(meanM,meanm), lty=2)
text(meanm,-0.1,"min",xpd="NA",cex=1.2)
text(meanM,-0.1,"max",xpd="NA",cex=1.2)
#Metric analysis
if (length(meanM==1)&mean(meanC)>0.2&mean(meanC)<0.7)
{
#parameters should be changed to detect the metrics following the desired method
Mdm=meanM
mdm=meanm
Mm=max(meanC)
SOStm=min(meanC)+SOSth*(Mm-min(meanC))
SOSm= mdm+which(meanC[mdm:Mdm]>SOStm)[1] -1 #Start of the Season
abline(v=c(Mdm,mdm), lty=2)
text(mdm,-0.1,"min",xpd="NA",cex=1.2)
text(Mdm,-0.1,"max",xpd="NA",cex=1.2)

for (i in 1:(length(year[,1])-1))
{
t=c(year[i,],year[i+1,1:16])
M=max(t) #maximum of the year
Md=which.max(t) #date of the maximum
if (Md>Mdm+5| Md<Mdm-5)
{ 
print(paste("max not in the seasonal windows year",rownames(year)[i]))
M=max(t[(Mdm-5):(Mdm+5)])
Md=which.max(t[(Mdm-5):(Mdm+5)])+(Mdm-5)-1
}
ml=min(t[1:Md]) #left minimum
mld=which.min(t[1:Md]) #date of left minimum
if (mld>mdm+5| mld<mdm-5)
{ 
print(paste("min not in the seasonal windows year",rownames(year)[i]))
ml=min(t[(mdm-5):(mdm+5)])
mld=which.min(t[(mdm-5):(mdm+5)])+(mdm-5)-1
}
mr=min(t[Md:(period+16)]) #right minimum
mrd=Md+which.min(t[Md:(period+16)])-1 #date of right minimum
SOSt=ml+SOSth*(M-ml)
SOS= mld+which(t[mld:Md]>SOSt)[1] -1 #Start of the Season
EOSt=mr+EOSth*(M-mr)
EOS= Md+which(t[Md:mrd]<EOSt)[1] -1 #End of the Season
LOS=EOS-SOS
cumNDVI=sum(t[EOS:SOS])
plot(seq(1,(period+16),1),t, type="l",xaxt="n",ylim=c(0,liM),xlab="time", ylab="SG filtered NDVI",main=paste(rownames(TS)[j],"NDVI Series",rownames(year)[i],"-",rownames(year)[i+1]))
axis(side=1,at=seq(1,(period+15),period/12),labels=rep(month.abb,2)[1:((period+16)/(period/12))])
points(x=Md, y=M, col="green", pch=3)
points(x=mld, y=ml, col="red", pch=3)
points(x=mrd, y=mr, col="red", pch=3)
points(x=SOS, y=t[SOS], col="blue", pch=3)
points(x=EOS, y=t[EOS], col="blue", pch=3)
arrows(SOS,0.05,EOS,0.05,code=3,length = 0.1)
arrows(SOS,-0.5,SOS,t[SOS],length = 0,lty=2)
arrows(EOS,-0.5,EOS,t[EOS],length = 0,lty=2)
text(SOS, -0.1, "SOS", xpd="NA")
text(EOS, -0.1, "EOS", xpd="NA")
text(mean(c(EOS, SOS)), 0.03, paste("cumNDVI =", as.character(format(cumNDVI, digits=5)), sep=""))
res=rbind(res,c(ml, mld, M, Md, SOSt, SOS, EOSt,EOS, LOS, cumNDVI, M-Mm, SOS-SOSm,mld-mdm ))
}
res=as.data.frame(res)
names(res)=c("ml", "mld", "M", "Md", "SOSt", "SOS", "EOSt","EOS", "LOS", "cumNDVI","diffMax","diffSOS","diffmd")
rownames(res)=Ystart:(Ystart+length(year[,1])-2)
#saving the metrics in a file
file=paste(rownames(TS)[j],outfile,sep="")
write.table(res,file,quote=F,row.names=F,sep="\t")
}
if (length(meanM==2)&mean(meanC)>0.2&mean(meanC)<0.7)
{
#dealing with 2 seasons
#minimum of the 2 minimums
mdm=meanm[which.min(meanC[meanm])]
mm=meanC[mdm]
#period to take into account
if (mdm-2*(period/12)<0)
{
st=mdm-2*(period/12)+period
yst=-1
} else {
st=mdm-2*(period/12)
yst=0
}
if (mdm+14*(period/12)>(2*period))
{
ed=mdm+14*(period/12)-2*period
yed=2
} else {
ed=mdm+14*(period/12)-period
yed=1
}
#max1 :
if (meanM[1]>mdm)
{Mdm=meanM[1] 
} else {
if (meanM[2]>mdm)
{ Mdm=meanM[2] 
} else {
Mdm=meanM[1]+period }}
#max2 :
Mdm2=meanM[!meanM%in%Mdm]
#min2 :
mdm2=meanm[!meanm%in%mdm]

for (i in (1-yst):(length(year[,1])-1))
{
if (yed-yst==1)
{
t=c(year[i+yst,st:period],year[i+yed,1:ed])
}
else
{
t=c(year[i+yst,st:period],year[i+((yed+yst)/2)],year[i+yed,1:ed])
}
#1st season
M1=max(t[(Mdm+st-2*(period/12)):(Mdm+st+2*(period/12))])#maximum of S1
Md1=which.max(t[(Mdm+st-2*(period/12)):(Mdm+st+2*(period/12))])+st-2*(period/12) #date of the maximum
if (Md1>meanM[1]+5| Md1<meanM[1]-5)
{ 
print(paste("max not in the seasonal windows year",rownames(year)[i]))
M=max(t[(Mdm-5):(Mdm+5)])
Md=which.max(t[(Mdm-5):(Mdm+5)])+(Mdm-5)-1
}
ml=min(t[1:Md]) #left minimum
mld=which.min(t[1:Md]) #date of left minimum
if (mld>mdm+5| mld<mdm-5)
{ 
print(paste("min not in the seasonal windows year",rownames(year)[i]))
ml=min(t[(mdm-5):(mdm+5)])
mld=which.min(t[(mdm-5):(mdm+5)])+(mdm-5)-1
}
mr=min(t[Md:(period+16)]) #right minimum
mrd=Md+which.min(t[Md:(period+16)])-1 #date of right minimum
SOSt=ml+SOSth*(M-ml)
SOS= mld+which(t[mld:Md]>SOSt)[1] -1 #Start of the Season
EOSt=mr+EOSth*(M-mr)
EOS= Md+which(t[Md:mrd]<EOSt)[1] -1 #End of the Season
LOS=EOS-SOS
cumNDVI=sum(t[EOS:SOS])
plot(seq(1,(period+16),1),t, type="l",xaxt="n",ylim=c(0,liM),xlab="time", ylab="SG filtered NDVI",main=paste(rownames(TS)[j],"NDVI Series",rownames(year)[i],"-",rownames(year)[i+1]))
axis(side=1,at=seq(1,(period+15),period/12),labels=rep(month.abb,2)[1:((period+16)/(period/12))])
points(x=Md, y=M, col="green", pch=3)
points(x=mld, y=ml, col="red", pch=3)
points(x=mrd, y=mr, col="red", pch=3)
points(x=SOS, y=t[SOS], col="blue", pch=3)
points(x=EOS, y=t[EOS], col="blue", pch=3)
arrows(SOS,0.05,EOS,0.05,code=3,length = 0.1)
arrows(SOS,-0.5,SOS,t[SOS],length = 0,lty=2)
arrows(EOS,-0.5,EOS,t[EOS],length = 0,lty=2)
text(SOS, -0.1, "SOS", xpd="NA")
text(EOS, -0.1, "EOS", xpd="NA")
text(mean(c(EOS, SOS)), 0.03, paste("cumNDVI =", as.character(format(cumNDVI, digits=5)), sep=""))
res=rbind(res,c(ml, mld, M, Md, SOSt, SOS, EOSt,EOS, LOS, cumNDVI, M-Mm, SOS-SOSm,mld-mdm ))
}
res=as.data.frame(res)
names(res)=c("ml", "mld", "M", "Md", "SOSt", "SOS", "EOSt","EOS", "LOS", "cumNDVI","diffMax","diffSOS","diffmd")
rownames(res)=Ystart:(Ystart+length(year[,1])-2)
#saving the metrics in a file
file=paste(rownames(TS)[j],outfile,sep="")
write.table(res,file,quote=F,row.names=F,sep="\t")
}
if (outgraph != FALSE)
{
dev.off()
}
}
return(res)
}