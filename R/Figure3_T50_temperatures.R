#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Makes Figure 3 (T50, temperatures)
#- merges air and leaf temperature datasets, also plots T50
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- read in the file of combined temperatures from HIEv.
dat.raw2 <- data.frame(data.table::fread("Data/WTC-TEMP_PARRA-CM-TEMPERATURES_COMBINED-20161010-20161123_L1.csv"))
dat.raw2$DateTime <- as.POSIXct(dat.raw2$DateTime,format="%Y-%m-%d %T",tz="GMT")
dat.raw2$Date <- as.Date(dat.raw2$DateTime)

dat <- subset(dat.raw2,as.Date(DateTime)>=as.Date("2016-10-29") & as.Date(DateTime)<=as.Date("2016-11-10"))

#- Tleaf vs. Tair
dat$Tdiff_IR <- with(dat,TargTempC_Avg-Tair_al)
dat$Tdiff_TC <- with(dat,Tleaf-Tair_al)

#- average across treatments for hourly averages
dat$DateTime <- nearestTimeStep(dat$DateTime, nminutes = 60, align = "floor")

dat.m <- summaryBy(.~DateTime+T_treatment+HW_Treatment,data=dat,FUN=c(mean,se),keep.names=T)
dat.m$combotrt <- factor(paste(dat.m$T_treatment,dat.m$HW_Treatment,sep="_"))



#- just the heatwave data
dat.hw <- subset(dat,as.Date(DateTime)>=as.Date("2016-10-31") & as.Date(DateTime)<=as.Date("2016-11-3"))
dat.hw$Date <- as.Date(dat.hw$DateTime)
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#- plots
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Figure 3a.
#- make the T50 plot. 
thermo <- read.csv("Data/WTC_TEMP-PARRA_CM_T50-CI_20161019-20161117_L1.csv")
thermo$Date <- as.Date(thermo$Date)
thermo$combotrt <- factor(paste(thermo$T_treatment,thermo$HW_treatment,sep="_"))

#- average across dates and treatments
thermo.m <- summaryBy(T50_mean~Date+combotrt,data=thermo,FUN=c(mean,se))

#- process the IR and air temperature data (daily mean and max). I may have screwed up the merging!
Tdat <- summaryBy(TargTempC_Avg+Tair_al~chamber+Date,data=dat.hw,FUN=c(mean,max))


#-- loop over each observation in the T50 dataset (thermo), get the temperature of some number of preceding days
ndays <- 1
thermo$TargTempC_Avg.mean <- thermo$Tair_al.mean <- thermo$TargTempC_Avg.max <- thermo$Tair_al.max <- NA
for (i in 1:nrow(thermo)){
  searchdate <- thermo$Date[i]
  mindate <- searchdate-ndays
  # inds <- which(Tdat$Date >=mindate & Tdat$Date <searchdate & Tdat$chamber == thermo$chamber[i])
  # thermo$TargTempC_Avg.mean[i] <- mean(Tdat[inds,"TargTempC_Avg.mean"],na.rm=T)
  # thermo$Tair_al.mean[i] <- mean(Tdat[inds,"Tair_al.mean"],na.rm=T)
  # thermo$TargTempC_Avg.max[i] <- mean(Tdat[inds,"TargTempC_Avg.max"],na.rm=T)
  # thermo$Tair_al.max[i] <- mean(Tdat[inds,"Tair_al.max"],na.rm=T)
  
  # switched to using dat.raw2 instead of Tdat
  inds <- which(dat.raw2$Date >=mindate & dat.raw2$Date <searchdate & dat.raw2$chamber == thermo$chamber[i])
  thermo$TargTempC_Avg.mean[i] <- mean(dat.raw2[inds,"TargTempC_Avg"],na.rm=T)
  thermo$Tair_al.mean[i] <- mean(dat.raw2[inds,"Tair_al"],na.rm=T)
  thermo$TargTempC_Avg.max[i] <- max(dat.raw2[inds,"TargTempC_Avg"],na.rm=T)
  thermo$Tair_al.max[i] <- max(dat.raw2[inds,"Tair_al"],na.rm=T)
  
}  

#- average across dates and treatments
thermo.m2 <- summaryBy(T50_mean~Date+HW_treatment,data=thermo,FUN=c(mean,se))

palette(c("blue","red"))

#-- plot T50 vs. time
#windows(50,60)
pdf("Output/Figure3a_T50.pdf",width=10.5)
par(mfrow=c(1,2),cex.lab=1.6,xpd=F,las=1,mar=c(5,7,3,1))
plotBy(T50_mean.mean~Date|HW_treatment,data=thermo.m2,type="o",pch=16,ylim=c(47,52),cex=1.5,legend=F,
       ylab=expression(Leaf~thermal~threshold~(T[50]*";"~degree*C)))

#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-4"))
rect(xleft=dates[1]+0.5,ybottom=40,xright=dates[2]+0.5,ytop=55,col="darkgrey",density=7) #add rectangles for droughts

adderrorbars(x=thermo.m2$Date,y=thermo.m2$T50_mean.mean,SE=thermo.m2$T50_mean.se,
             direction="updown",col=thermo.m2$HW_treatment,barlen=0.05)
legend("topright",xpd=NA,pch=16,col=palette()[1:2],ncol=1,cex=1.2,
       legend=c("Control","Heatwave"))
plotBy(T50_mean.mean~Date|HW_treatment,data=thermo.m2,type="o",pch=16,ylim=c(47,52),cex=1.5,legend=F,add=T,
       ylab=expression(Leaf~thermotolerance~(T[50]*";"~degree*C)))
legend("topleft",legend=letters[1],cex=1.4,bty="n")
text(x=thermo.m2$Date[1]+5,y=47,"Pre-heatwave",cex=1.2)
text(x=thermo.m2$Date[1]+23,y=47,"Post-heatwave",cex=1.2)

#- plot T50 vs. leaf temperature
plotBy(T50_mean~TargTempC_Avg.mean|HW_treatment,data=thermo,type="p",pch=16,ylim=c(47,52),cex=1.5,legend=F,
       ylab=expression(Leaf~thermal~threshold~(T[50]*";"~degree*C)),
       xlab=expression(Canopy~temperature~(T[L-IR]*";"~degree*C)))
legend("topleft",legend=letters[2],cex=1.4,bty="n")
lm1 <- lm(T50_mean~TargTempC_Avg.mean,data=thermo)
ablineclip(lm1,x1=min(thermo$TargTempC_Avg.mean),x2=max(thermo$TargTempC_Avg.mean))
ellipse(c(21,50.2),shape=matrix(c(12,0.1,0.1,12),nrow=2,ncol=2),radius=c(0.7,0.2))

dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the flux data, calculate what leaf temperatures SHOULD have been, to add to plot of Tleaf vs. Tair
dat <- read.csv("Data/WTC_TEMP-PARRA_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv")
dat$DateTime_hr <- as.POSIXct(dat$DateTime_hr,format="%Y-%m-%d %T",tz="GMT")
names(dat)[4] <- "HW_Treatment"

#- aggregate to hourly averages
dat$DateTime_hr <- nearestTimeStep(dat$DateTime_hr,nminutes=60,"floor")
dat.hr <- summaryBy(.~DateTime_hr+chamber+T_treatment+HW_Treatment+combotrt,FUN=mean,keep.names=T,data=dat)

#- subset to just the heatwave timeperiods
starttime <- as.POSIXct("2016-10-31 00:00:00",format="%Y-%m-%d %T",tz="GMT")
endtime <- as.POSIXct("2016-11-04 20:00:00",format="%Y-%m-%d %T",tz="GMT")

dat <- subset(dat.hr,HW_Treatment=="HW" & DateTime_hr>starttime & DateTime_hr < endtime & Tair_al>15)
test <- subset(dat,PPFD_Avg >500)
g0 <- 0.003
#- model the predicted leaf temperatures. include an extra point at max air temperature
pred1 <- suppressMessages(PhotosynEB(Tair=c(test$Tair_al,44.6),VPD=c(test$VPD,5.77),Wind=8,Wleaf=0.01,StomatalRatio=1,
                                     LeafAbs=0.95,
                                     PPFD=c(test$PPFD_Avg,1800),g1=1.9,g0=g0,
                                     Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
                                     Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- Figure 3b

#- plot IR leaf temperatures relative to air temperature, as density plot 

pdf("Output/Figure3b_Tair_Tleaf.pdf")
par(mar=c(5,6,1,1))
blues.ramp <- colorRampPalette(brewer.pal(9,"Blues")[-1],alpha=T)
reds.ramp <- colorRampPalette(brewer.pal(9,"Reds")[-1],alpha=T)
ptsize <- 1.2

#- extract a subset of the dat
Tleaf.c <- subset(dat.hw,HW_Treatment=="C" & PPFD_Avg>=500 & DateTime > starttime & DateTime < endtime)
Tleaf.hw <- subset(dat.hw,HW_Treatment=="HW" &PPFD_Avg>=500 & DateTime > starttime & DateTime < endtime)

#- plot Tleaf vs. Tair. 
shading <- 0.5
dhw <- alpha(densCols(c(Tleaf.hw$TargTempC_Avg,Tleaf.hw$Tleaf),c(Tleaf.hw$Tair_al,Tleaf.hw$Tair_al),colramp=reds.ramp),alpha=shading)
plot(c(Tleaf.hw$TargTempC_Avg,Tleaf.hw$Tleaf)~c(Tleaf.hw$Tair_al,Tleaf.hw$Tair_al),col=dhw,pch=16,cex=ptsize,
       ylim=c(10,50),xlim=c(10,45),xaxt="n",yaxt="n",
     xlab=expression(T[air]~(degree*C)),ylab=expression(T[leaf]~(degree*C)),cex.lab=2)
dc <- alpha(densCols(c(Tleaf.c$TargTempC_Avg,Tleaf.c$Tleaf),c(Tleaf.c$Tair_al,Tleaf.c$Tair_al),colramp=blues.ramp),alpha=shading)

points(c(Tleaf.c$TargTempC_Avg,Tleaf.c$Tleaf)~c(Tleaf.c$Tair_al,Tleaf.c$Tair_al),col=dc,pch=16,cex=ptsize,
     ylim=c(10,55),xlim=c(10,55),xaxt="n",yaxt="n",
     xlab=expression(T[air]~(degree*C)),ylab=expression(T[leaf]~(degree*C)))

axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T,las=1);axis(4,tck=0.025,labels=F)


abline(0,1,lty=3,lwd=3,col="darkgrey")
lm1 <- lm(TargTempC_Avg~Tair_al+I(Tair_al^2),data=subset(dat.hw,PPFD_Avg>=500))
legend("topleft",legend=letters[2],cex=1.4,bty="n")

xvals <-seq(11,45,length.out=101)
predictions <- predict.lm(lm1,newdata=data.frame(Tair_al = seq(11,45,length.out=101)),interval="prediction")
lines(predictions[,1]~seq(11,45,length.out=101),col="black",lwd=2)
lines(predictions[1:23,1]+0.1~xvals[1:23],col="red",lwd=2) 

#lines(predictions[,2]~seq(11,45,length.out=101),col="black",lwd=1,lty=2)
#lines(predictions[,3]~seq(11,45,length.out=101),col="black",lwd=1,lty=2)

#- add the "predicted" Tleaf values in the absence of transpirational cooling
lines(pred1$Tleaf~pred1$Tair,lty=1,lwd=2,col="red")


legend(x=31,y=22,pch=c(16,16,NA,NA,NA),lty=c(NA,NA,3,1,1),lwd=c(NA,NA,2,2,2),pt.cex=ptsize*1.5,
       legend=c("Control","Heatwave","1:1","Fit","Model prediction"),
       col=c(dc[1],dhw[1],"darkgrey","black","red"),bty="n",cex=1.2)
dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Figure 3 c-d.

#- calculate daily maximum Tleaf (IR) and Tair during the heatwave


#- calculate daily maximums, sunlit periods only
dat.hw.day <- summaryBy(TargTempC_Avg+Tair_al~chamber+HW_Treatment+Date,
                        data=subset(dat.hw,PPFD_Avg>10),FUN=max,keep.names=T)

#- calculate mean maximums for each treatment
summaryBy(TargTempC_Avg+Tair_al~HWtrt,data=dat.hw.day,FUN=mean)

#- get the hour of teh day
dat.hw$hour <- hour(dat.hw$DateTime)

#- histogram of temperatures during the heatwave (IR)
hw_Temps <- subset(dat.hw,HW_Treatment=="HW" & PPFD_Avg > 1000 & hour >= 11 & hour <= 18)$TargTempC_Avg
amb_Temps <- subset(dat.hw,HW_Treatment=="C" & PPFD_Avg > 1000 & hour >= 11 & hour <= 18)$TargTempC_Avg

#- histogram of temperatures during the heatwave (both IR and TC)
hw_Temps <- c(subset(dat.hw,HW_Treatment=="HW" & PPFD_Avg > 500 & hour >= 12 & hour <= 16)$TargTempC_Avg,subset(dat.hw,HW_Treatment=="HW" & PPFD_Avg > 500 & hour >= 12 & hour <= 16)$LeafT_Avg.1.,subset(dat.hw,HW_Treatment=="HW" & PPFD_Avg > 500 & hour >= 12 & hour <= 16)$LeafT_Avg.2.)
amb_Temps <- c(subset(dat.hw,HW_Treatment=="C" & PPFD_Avg > 500 & hour >= 12 & hour <= 16)$TargTempC_Avg,subset(dat.hw,HW_Treatment=="C" & PPFD_Avg > 500 & hour >= 12 & hour <= 16)$LeafT_Avg.1.,subset(dat.hw,HW_Treatment=="C" & PPFD_Avg > 500 & hour >= 12 & hour <= 16)$LeafT_Avg.2.)


pdf("Output/Figure3cd-Tair_Tleaf.pdf",width=4)
par(mfrow=c(2,1),mar=c(2.5,0,0,0),oma=c(2,1,1,8),las=1,cex.lab=2)
ylims=c(0,0.25)
hist(amb_Temps,xlim=c(10,55),main="",freq=F,xlab="",yaxt="n",xaxs="i", yaxs="i",ylim=ylims,col="blue");box()
legend("top","Control",bty="n")
axis(4)
legend("topleft",legend=letters[3],cex=1.4,bty="n")
abline(v=48.5,lty=2,col="blue")

hist(hw_Temps,xlim=c(10,55),main="",freq=F,xlab="",yaxt="n",xaxs="i",yaxs="i",ylim=ylims,col="red");box()
title(xlab=expression(T[leaf]~(degree*C)),outer=T,xpd=NA,line=0.7)
abline(v=48.5,lty=2,col="blue")
abline(v=51.5,lty=2,col="red")
legend("top","Heatwave",bty="n")
axis(4)
legend("topleft",legend=letters[4],cex=1.4,bty="n")

#title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=1.5,line=0.5)
title(ylab=expression(Density~of~T[leaf]),outer=T,cex.lab=2,line=-17)
dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- did Tleaf exceed the pre-heatwave T50? Yes, for some chambers

#- pull out the maximum temperatures for each chamber
max_temps_IR <- subset(summaryBy(TargTempC_Avg~chamber+HW_Treatment,data=dat.hw,FUN=max,na.rm=T),HW_Treatment!="C")
max_temps_Tleaf1 <- subset(summaryBy(LeafT_Avg.1.~chamber+HW_Treatment,data=dat.hw,FUN=max,na.rm=T),HW_Treatment!="C")
max_temps_Tleaf2 <- subset(summaryBy(LeafT_Avg.2.~chamber+HW_Treatment,data=dat.hw,FUN=max,na.rm=T),HW_Treatment!="C")

names(max_temps_IR) <- c("chamber","HW_treatment","Tleaf")
names(max_temps_Tleaf1) <- c("chamber","HW_treatment","Tleaf")
names(max_temps_Tleaf2) <- c("chamber","HW_treatment","Tleaf")

max_Tleaf_3 <- rbind(max_temps_IR,max_temps_Tleaf1,max_temps_Tleaf2)

max_Tleaf <- summaryBy(Tleaf~chamber,data=max_Tleaf_3,FUN=max)

#- get T50 prior to the heatwave
thermo.pre <- summaryBy(T50_mean~chamber,
                        data=subset(thermo,HW_treatment!="control" & Date==as.Date("2016-10-19")),
                                    FUN=mean,keep.names=T)
thermo.post <- summaryBy(T50_mean~chamber,
                        data=subset(thermo,HW_treatment!="control"),
                        FUN=max,keep.names=T)
names(thermo.post) <- c("chamber","T50_max")
compare1 <- merge(max_Tleaf,thermo.pre,by="chamber")
compare <- merge(compare1,thermo.post,by="chamber")
compare$exceed_pre <- ifelse(compare$Tleaf.max>compare$T50_mean,"yes","no")
compare$exceed_max <- ifelse(compare$Tleaf.max>compare$T50_max,"yes","no")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- make a supplementary figure showing the met data during the flux period

#- read in the flux data
wtc1 <- read.csv("Data/WTC_TEMP-PARRA_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv")
#wtc1 <- read.csv("Data/WTC_TEMP-PARRA_WTCFLUX_20161028-20161115_L0.csv")
wtc1$DateTime <- as.POSIXct(wtc1$DateTime,format="%Y-%m-%d %T",tz="GMT")
#wtc1$VPD <- RHtoVPD(RH=wtc1$RH_al,TdegC=wtc1$Tair_al)
wtc1$DateTime_hr <- nearestTimeStep(wtc1$DateTime,nminutes=60,align="floor")

#starttime <- as.POSIXct("2016-10-29 00:00:00",tz="GMT")
#stoptime <- as.POSIXct("2016-11-11 00:00:00",tz="GMT")
#dat <- subset(dat,as.Date(DateTime_hr)>=as.Date("2016-10-29") & as.Date(DateTime_hr)<=as.Date("2016-11-10"))

wtc <- subset(wtc1,as.Date(DateTime_hr)>=as.Date("2016-10-29") & as.Date(DateTime_hr)<=as.Date("2016-11-10"))

#-- create hourly values for subsequent averaging of fluxes
wtc$DateTime_hr <- nearestTimeStep(wtc$DateTime,nminutes=60,align="floor")

#- average PAR, VPD, FLuxCO2 and FLuxH2O for each chamber
wtc.m1 <- summaryBy(PAR+VPD+Tair_al~DateTime_hr+chamber+T_treatment,data=wtc,
                    FUN=mean,keep.names=T,na.rm=T)


#- merge in the heatwave treatments and total leaf area data
linkdf <- data.frame(chamber = levels(as.factor(wtc$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08

wtc.m <- merge(wtc.m1,linkdf,by="chamber")
wtc.m$combotrt <- factor(paste(wtc.m$T_treatment,wtc.m$HWtrt,sep="_"))

#- get treatment averages for plotting
wtc.trt <- summaryBy(PAR+VPD+Tair_al~DateTime_hr+combotrt,data=wtc.m,
                     FUN=c(mean,se),na.rm=T)

#-

#----
#- plot PAR, VPD, and temperatures
#windows(70,80)
pdf("Output/FigureS3-Met.pdf",width=6)
par(mfrow=c(7,1),mar=c(0,6,0,6),oma=c(7,0,3,0),cex.lab=1.6,xpd=F,las=1)
palette(c("blue","black","orange","red"))

#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 06:00:00","2016-11-4 04:00:00"),format="%Y-%m-%d %T",tz="GMT")


#- plot PAR
plotBy(PAR.mean~DateTime_hr,data=subset(wtc.trt,combotrt=="ambient_C"),legend=F,col="darkgrey",type="l",lwd=2,
       ylim=c(0,2000),ylab="PPFD",xaxt="n");axis(side=4)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(wtc.trt$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend(x=subset(wtc.trt,combotrt=="ambient_C")$DateTime_hr[1]-200000,
       y=2700,xpd=NA,lwd=3,col=palette()[1:4],ncol=4,cex=0.9,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW

#- plot VPD
plotBy(VPD.mean~DateTime_hr|combotrt,data=wtc.trt,legend=F,col=palette()[1:4],type="l",lwd=2,
       ylim=c(0,6),ylab="VPD",xaxt="n");axis(side=4)
adderrorbars(x=wtc.trt$DateTime_hr,y=wtc.trt$VPD.mean,SE=wtc.trt$VPD.se,direction="updown",col=wtc.trt$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(wtc.trt$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[2],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW


#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|combotrt,data=wtc.trt,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(5,50),
       ylab=expression(T[air]),xaxt="n");axis(side=4)
adderrorbars(x=wtc.trt$DateTime_hr,y=wtc.trt$Tair_al.mean,SE=wtc.trt$Tair_al.se,direction="updown",col=wtc.trt$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(wtc.trt$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[3],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW


#- plot Tleaf (IR)
plotBy(TargTempC_Avg.mean~DateTime|combotrt,data=dat.m,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(5,50),
       ylab=expression(T[L-IR]),xaxt="n");axis(side=4)
adderrorbars(x=dat.m$DateTime,y=dat.m$TargTempC_Avg.mean,SE=dat.m$TargTempC_Avg.se,direction="updown",col=dat.m$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[4],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW

#- plot Tleaf (TC)
plotBy(Tleaf.mean~DateTime|combotrt,data=dat.m,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(5,50),
       ylab=expression(T[L-TC]),xaxt="n");axis(side=4)
adderrorbars(x=dat.m$DateTime,y=dat.m$Tleaf.mean,SE=dat.m$Tleaf.se,direction="updown",col=dat.m$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[5],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW

#- plot Tleaf diferrence (IR)
plotBy(Tdiff_IR.mean~DateTime|combotrt,data=dat.m,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(-2,7),
       ylab=expression(T[L-IR]-T[air]),xaxt="n");axis(side=4);abline(h=0)
adderrorbars(x=dat.m$DateTime,y=dat.m$Tdiff_IR.mean,SE=dat.m$Tdiff_IR.se,direction="updown",col=dat.m$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[6],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW

#- plot Tleaf diferrence (TC)
plotBy(Tdiff_TC.mean~DateTime|combotrt,data=dat.m,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(-2,7),
       ylab=expression(T[L-TC]-T[air]),xaxt="n");axis(side=4);abline(h=0)
adderrorbars(x=dat.m$DateTime,y=dat.m$Tdiff_TC.mean,SE=dat.m$Tdiff_TC.se,direction="updown",col=dat.m$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime,tz="GMT"))),las=3,cex.axis=1.5,labels=T)
legend("topright",legend=letters[7],cex=1.4,bty="n")
rect(xleft=dates[1],ybottom=-50,xright=dates[2],ytop=3000,col="darkgrey",density=7) #add rectangles for HW
dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# statistical analysis of T50
head(thermo)
thermo$DateFac<- as.factor(thermo$Date)

#- fit model 
F1 <- lme(T50_mean~T_treatment*HW_treatment*DateFac,random=list(~1|chamber),
          data=thermo,method="REML")

#look at model diagnostics
# plot(F1,resid(.,type="p")~fitted(.) | T_treatment,abline=0)   #resid vs. fitted for each treatment
# plot(F1,T50_mean~fitted(.)|chamber,abline=c(0,1))           #predicted vs. fitted for each chamber
# plot(F1,T50_mean~fitted(.),abline=c(0,1))                   #predicted vs. fitted
# qqnorm(F1, ~ resid(., type = "p"), abline = c(0, 1))          #qqplot. 
# hist(F1 $residuals[,1])
anova(F1,type="marginal")

#post-hoc test
testInteractions(F1, fixed=c("DateFac"), across=c("HW_treatment")) #simple effect of HW at each date
interactionMeans(F1, factors=c("HW_treatment","DateFac"))









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- What were the mean (min/max) temperatures in the experiment? Read in the full flux data
# 
# flux <- data.frame(data.table::fread("C:/Repos/wtc4_flux_processing/output/WTC_TEMP-PARRA_WTCFLUX_20160228-20161030_L0.csv"))
# flux$DateTime <- as.POSIXct(flux$DateTime,format="%Y-%m-%d %T",tz="GMT")
# plot(Tair_al~DateTime,data=subset(flux,T_treatment=="ambient"))
# 
# #- remove very cold outlier
# flux$Tair_al[which(flux$Tair_al < -15)] <- NA
# 
# #- remove very hot outlier
# flux$Tair_al[which(flux$Tair_al > 60)] <- NA
# 
# summaryBy(Tair_al~T_treatment,data=flux,FUN=c(mean,min,max),na.rm=T)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the percentage of leaves damaged by the heatwave. (data need uploading to HIEv)
harvest <- read.csv("Data/WTC_TEMP-PARRA_CM_CANOPY-HARVEST-HEATWAVE_20161121_L0.csv")
harvest$LeafArea <- with(harvest,Leaf_DW*SLA/10000) # calculate total leaf area (m2) for each canopy layer
harvest$LeafArea_damage <- with(harvest,HW_damage_DW*SLA/10000) # calculate total leaf area (m2) for each canopy layer damaged by the heatwave
harvest.sum <- summaryBy(LeafArea+LeafArea_damage~chamber,data=harvest,FUN=sum,keep.names=T) # sumacross the three canopy layers

linkdf <- data.frame(chamber = levels(as.factor(dat.hw.day$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
harvest.sum <- merge(harvest.sum,linkdf,by="chamber")

#- % canopy damaged
harvest.sum$prop <- with(harvest.sum,LeafArea_damage/LeafArea)*100
summaryBy(prop~HWtrt,data=harvest.sum)

#- make barchart of damage
pdf("Output/FigureS4_damage.pdf")
par(mar=c(6,6,1,1))
damage.m <- summaryBy(prop~HWtrt,data=harvest.sum,FUN=c(mean,se))
xvals <- barplot(damage.m$prop.mean, beside=T, ylab="", names.arg=rep("",2),yaxt="n",space=0,
                 cex.names=1.5, las=1, ylim=c(0,10), col=c("blue","red"))#col=brewer.pal(6,"Set1")[3:6])
adderrorbars(x=xvals,y=damage.m$prop.mean,SE=damage.m$prop.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(2,las=1)
title(ylab=expression(Leaf~area~damaged~("%")),outer=F,cex.lab=2) # y-axis label
text(x=xvals+0.2,y=-0.8,labels=c("Control","Heatwave"),xpd=T,srt=0,pos=2,cex=1.5,xpd=NA,cex.lab=1.1)
dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------


