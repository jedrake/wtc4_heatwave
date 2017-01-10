#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- merges air and leaf temperature datasets, also plots T50
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

source("R/loadLibraries.R")



#-----------------------------------------------------------------------------------------------------------
#- download the leaf temperature data from HIEv (i.e., the actual thermocouples!)
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_LEAFTEMPS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{2}_LEAFTEMPS", startDate="2016-10-10", endDate="2016-11-30",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_Tleaf.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
d$DateTime_hr <- nearestTimeStep(d$DateTime, nminutes = 15, align = "floor")

#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- download the AIRVARS data (PPFD and IR-T data), do some manipulation
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_AIRVARS")
IRT <- downloadTOA5("WTC_AUTO_C[0-9]{2}_AIRVARS", startDate="2016-10-10", endDate="2016-11-30",
                    topath="C:/Repos/wtc4_flux/data/fromHIEv",
                    cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache.rdata")
IRT$chamber <- as.factor(substr(IRT$Source,start=10,stop=12)) # extract the chamber number from the filename
IRT$T_treatment <- ifelse(as.numeric(substr(as.character(IRT$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
IRT$Tdiff <- with(IRT,TargTempC_Avg-SBTempC_Avg)
IRT$DateTime_hr <- nearestTimeStep(IRT$DateTime, nminutes = 15, align = "floor")
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- load the trendlogs
source("C:/Repos/wtc4_flux_processing/R/getWTCtrendlog.R")
load("C:/Repos/wtc4_flux_processing/data/TrendlogChDF.RData")
load("C:/Repos/wtc4_flux_processing/data/TrendlogRefDF.RData")
Trend.dat <- subset(TrendlogChDF,as.Date(DateTime)>=as.Date("2016-10-10"))
Trend.dat$DateTime_hr <- nearestTimeStep(Trend.dat$DateTime, nminutes = 15, align = "floor")

#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- merge the datasets
d.dat <- d[,c("chamber","T_treatment","DateTime_hr","LeafT_Avg.1.","LeafT_Avg.2.")]
d.dat.m <- data.frame(dplyr::summarize(dplyr::group_by(d.dat, DateTime_hr, chamber,T_treatment), 
                                       LeafT_Avg.1.=mean(LeafT_Avg.1.,na.rm=T),
                                       LeafT_Avg.2.=mean(LeafT_Avg.2.,na.rm=T)))


IRT.dat <- IRT[,c("chamber","DateTime_hr","PPFD_Avg","TargTempC_Avg")]
IRT.dat.m <- data.frame(dplyr::summarize(dplyr::group_by(IRT.dat, DateTime_hr, chamber), 
                                        TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                                        PPFD_Avg=mean(PPFD_Avg,na.rm=T)))


Trend.dat <- Trend.dat[,c("chamber","DateTime_hr","Tair_al","RH_al")]
Trend.dat.m <- data.frame(dplyr::summarize(dplyr::group_by(Trend.dat, DateTime_hr, chamber), 
                                           Tair_al =mean(Tair_al ,na.rm=T),
                                           RH_al=mean(RH_al,na.rm=T)))
Trend.dat.m$VPD <- RHtoVPD(RH=Trend.dat.m$RH_al,TdegC=Trend.dat.m$Tair_al)

dat2 <- merge(d.dat.m,IRT.dat.m,by=c("chamber","DateTime_hr"))
dat.raw <- merge(dat2,Trend.dat.m,by=c("chamber","DateTime_hr"))

linkdf <- data.frame(chamber = levels(as.factor(dat.raw$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
dat.raw2 <- merge(dat.raw,linkdf,by="chamber")
dat.raw2$combotrt <- factor(paste(dat.raw2$T_treatment,dat.raw2$HWtrt,sep="_"))
dat.raw2$Tleaf <- rowMeans(dat.raw2[,c("LeafT_Avg.1.","LeafT_Avg.2.")],na.rm=T)

dat.raw2$Tleafmean <- rowMeans(dat.raw2[,c("Tleaf","TargTempC_Avg")])

dat <- subset(dat.raw2,as.Date(DateTime_hr)>=as.Date("2016-10-29") & as.Date(DateTime_hr)<=as.Date("2016-11-10"))

#- average across treatments
dat.m <- summaryBy(.~DateTime_hr+T_treatment+HWtrt,data=dat,FUN=c(mean,se),keep.names=T)
dat.m$combotrt <- factor(paste(dat.m$T_treatment,dat.m$HWtrt,sep="_"))

#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#- plots
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- plot IR leaf temperatures relative to air temperature


windows(60,60)
par(mfrow=c(1,1),mar=c(6,6,1,1),oma=c(0,0,1,0),cex.lab=1.6)

plotBy(TargTempC_Avg~Tair_al|combotrt,data=subset(dat,PPFD_Avg>500),legend=F,las=1,
       xlab=expression(T[air]~(degree*C)),
       ylab=expression(Infrared~T[leaf]~(degree*C)))
legend("topleft",xpd=NA,pch=16,col=palette()[1:4],ncol=1,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
abline(0,1,lty=3,lwd=3)
lm1 <- lm(TargTempC_Avg~Tair_al,data=subset(dat,PPFD_Avg>500))
abline(lm1,lwd=3)

legend("bottomright",lty=c(3,1),lwd=c(3,3),legend=c("1:1","Fit"),bty="n",cex=1.5)
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate daily maximum Tleaf (IR) and Tair during the heatwave
dat.hw <- subset(dat,as.Date(DateTime_hr)>=as.Date("2016-10-31") & as.Date(DateTime_hr)<=as.Date("2016-11-3"))
dat.hw$Date <- as.Date(dat.hw$DateTime_hr)

#- calculate daily maximums
dat.hw.day <- summaryBy(TargTempC_Avg+Tair_al~chamber+HWtrt+Date,data=dat.hw,FUN=max,keep.names=T)

#- calculate mean maximums for each treatment
summaryBy(TargTempC_Avg+Tair_al~HWtrt,data=dat.hw.day,FUN=mean)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the percentage of leaves damaged by the heatwave
harvest <- read.csv("Data/Harvest/WTC_TEMP_CM_PARRA_CANOPY-HARVEST_20161121_L0.csv")
harvest$LeafArea <- with(harvest,Leaf_DW*SLA/10000) # calculate total leaf area (m2) for each canopy layer
harvest$LeafArea_damage <- with(harvest,HW_damage_DW*SLA/10000) # calculate total leaf area (m2) for each canopy layer damaged by the heatwave
harvest.sum <- summaryBy(LeafArea+LeafArea_damage~chamber,data=harvest,FUN=sum,keep.names=T) # sumacross the three canopy layers

linkdf <- data.frame(chamber = levels(as.factor(dat.hw.day$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
harvest.sum <- merge(harvest.sum,linkdf,by="chamber")

#- % canopy damaged
harvest.sum$prop <- with(harvest.sum,LeafArea_damage/LeafArea)*100
summaryBy(prop~HWtrt,data=harvest.sum)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- make a supplementary figure showing the met data during the flux period

#- read in the flux data
wtc1 <- read.csv("Data/WTC_TEMP-PARRA_WTCFLUX_20161028-20161115_L0.csv")
wtc1$DateTime <- as.POSIXct(wtc1$DateTime,format="%Y-%m-%d %T",tz="GMT")
wtc1$VPD <- RHtoVPD(RH=wtc1$RH_al,TdegC=wtc1$Tair_al)
wtc1$DateTime_hr <- nearestTimeStep(wtc1$DateTime,nminutes=60,align="floor")

#starttime <- as.POSIXct("2016-10-29 00:00:00",tz="GMT")
#stoptime <- as.POSIXct("2016-11-11 00:00:00",tz="GMT")
#dat <- subset(dat,as.Date(DateTime_hr)>=as.Date("2016-10-29") & as.Date(DateTime_hr)<=as.Date("2016-11-10"))

wtc <- subset(wtc1,as.Date(DateTime_hr)>=as.Date("2016-10-29") & as.Date(DateTime_hr)<=as.Date("2016-11-10") & DoorCnt == 0)

#-- create hourly values for subsequent averaging of fluxes
wtc$DateTime_hr <- nearestTimeStep(wtc$DateTime,nminutes=60,align="floor")

#- average PAR, VPD, FLuxCO2 and FLuxH2O for each chamber
wtc.m1 <- summaryBy(PAR+VPD+Tair_al~DateTime_hr+chamber+T_treatment,data=subset(wtc,DoorCnt==0),
                    FUN=mean,keep.names=T,na.rm=T)


#- merge in the heatwave treatments and total leaf area data
linkdf <- data.frame(chamber = levels(as.factor(wtc$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08

wtc.m <- merge(wtc.m1,linkdf,by="chamber")
wtc.m$combotrt <- factor(paste(wtc.m$T_treatment,wtc.m$HWtrt,sep="_"))

#- get treatment averages for plotting
wtc.trt <- summaryBy(PAR+VPD+Tair_al~DateTime_hr+combotrt,data=wtc.m,
                     FUN=c(mean,se),na.rm=T)


#----
#- plot PAR, VPD, and temperatures
windows(70,60)
par(mfrow=c(5,1),mar=c(0,6,0,6),oma=c(7,0,3,0),cex.lab=1.6,xpd=F,las=1)
palette(c("blue","black","orange","red"))

#- plot PAR
plotBy(PAR.mean~DateTime_hr,data=subset(wtc.trt,combotrt=="ambient_C"),legend=F,col="darkgrey",type="l",lwd=2,
       ylim=c(0,2000),ylab="PPFD",xaxt="n");axis(side=4)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend(x=subset(wtc.trt,combotrt=="ambient_C")$DateTime_hr[1]-120000,
       y=2600,xpd=NA,lwd=3,col=palette()[1:4],ncol=4,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=1.4,bty="n")

#- plot VPD
plotBy(VPD.mean~DateTime_hr|combotrt,data=wtc.trt,legend=F,col=palette()[1:4],type="l",lwd=2,
       ylim=c(0,6),ylab="VPD",xaxt="n");axis(side=4)
adderrorbars(x=wtc.trt$DateTime_hr,y=wtc.trt$VPD.mean,SE=wtc.trt$VPD.se,direction="updown",col=wtc.trt$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[2],cex=1.4,bty="n")


#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|combotrt,data=wtc.trt,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(5,50),
       ylab=expression(T[air]),xaxt="n");axis(side=4)
adderrorbars(x=wtc.trt$DateTime_hr,y=wtc.trt$Tair_al.mean,SE=wtc.trt$Tair_al.se,direction="updown",col=wtc.trt$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[3],cex=1.4,bty="n")

#- plot Tleaf (IR)
plotBy(TargTempC_Avg.mean~DateTime_hr|combotrt,data=dat.m,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(5,50),
       ylab=expression(T[leaf-IR]),xaxt="n");axis(side=4)
adderrorbars(x=dat.m$DateTime_hr,y=dat.m$TargTempC_Avg.mean,SE=dat.m$TargTempC_Avg.se,direction="updown",col=dat.m$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5,labels=F)
legend("topright",legend=letters[4],cex=1.4,bty="n")

#- plot Tleaf (TC)
plotBy(Tleaf.mean~DateTime_hr|combotrt,data=dat.m,legend=F,col=palette()[1:4],type="l",lwd=2,ylim=c(5,50),
       ylab=expression(T[leaf-TC]),xaxt="n");axis(side=4)

adderrorbars(x=dat.m$DateTime_hr,y=dat.m$Tleaf.mean,SE=dat.m$Tleaf.se,direction="updown",col=dat.m$combotrt,barlen=0)
axis.POSIXct(side=1,at=as.POSIXct(unique(as.Date(dat.m$DateTime_hr,tz="GMT"))),las=3,cex.axis=1.5)
legend("topright",legend=letters[5],cex=1.4,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- make the T50 plots
thermo <- read.csv("Data/WTC_TEMP-PARRA_CM_T50-CI_20161019-20161117_L1.csv")
thermo$Date <- as.Date(thermo$Date)
thermo$combotrt <- factor(paste(thermo$T_treatment,thermo$HW_treatment,sep="_"))

#- average across dates and treatments
thermo.m <- summaryBy(T50_mean~Date+combotrt,data=thermo,FUN=c(mean,se))



#---- WORK on ThiS!
#- process the IR and air temperature data (daily mean and max). I may have screwed up the merging!


dat.raw2 <- merge(IRT.dat.m,Trend.dat.m,by=c("chamber","DateTime_hr"))

dat.raw2$Date <- as.Date(dat.raw2$DateTime_hr)
Tdat <- summaryBy(TargTempC_Avg+Tair_al~chamber+Date,data=dat.raw2,FUN=c(mean,max))


#-- loop over each observation in the T50 dataset (thermo), get the temperature of some number of preceding days
ndays <- 1
thermo$TargTempC_Avg.mean <- thermo$Tair_al.mean <- thermo$TargTempC_Avg.max <- thermo$Tair_al.max <- NA
for (i in 1:nrow(thermo)){
  searchdate <- thermo$Date[i]
  mindate <- searchdate-ndays
  inds <- which(Tdat$Date >=mindate & Tdat$Date <searchdate & Tdat$chamber == thermo$chamber[i])
  thermo$TargTempC_Avg.mean[i] <- mean(Tdat[inds,"TargTempC_Avg.mean"],na.rm=T)
  thermo$Tair_al.mean[i] <- mean(Tdat[inds,"Tair_al.mean"],na.rm=T)
  thermo$TargTempC_Avg.max[i] <- mean(Tdat[inds,"TargTempC_Avg.max"],na.rm=T)
  thermo$Tair_al.max[i] <- mean(Tdat[inds,"Tair_al.max"],na.rm=T)
  
}  

#---
#-- plot T50 vs. time
windows(100,60)
par(mfrow=c(1,2),cex.lab=1.6,xpd=F,las=1,mar=c(5,7,3,1))
plotBy(T50_mean.mean~Date|combotrt,data=thermo.m,type="o",pch=16,ylim=c(47,52),cex=1.5,legend=F,
       ylab=expression(Leaf~thermotolerance~(T[50]*";"~degree*C)))

#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-4"))
rect(xleft=dates[1]+0.5,ybottom=40,xright=dates[2]+0.5,ytop=55,col="darkgrey",density=7) #add rectangles for droughts

adderrorbars(x=thermo.m$Date,y=thermo.m$T50_mean.mean,SE=thermo.m$T50_mean.se,
             direction="updown",col=thermo.m$combotrt,barlen=0.05)
legend(x=dates[2]-15,y=52.75,xpd=NA,pch=16,col=palette()[1:4],ncol=4,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
plotBy(T50_mean.mean~Date|combotrt,data=thermo.m,type="o",pch=16,ylim=c(47,52),cex=1.5,legend=F,add=T,
       ylab=expression(Leaf~thermotolerance~(T[50]*";"~degree*C)))
legend("topleft",legend=letters[1],cex=1.4,bty="n")


#---
#-- plot T50 vs. the mean leaf T of the preceding day
plotBy(T50_mean~TargTempC_Avg.mean|combotrt,data=thermo,type="p",pch=16,ylim=c(47,52),cex=1.5,legend=F,
       ylab=expression(Leaf~thermotolerance~(T[50]*";"~degree*C)),
       xlab=expression(Mean~T[leaf]~of~preceding~day~(degree*C)))
lm2 <- lm(T50_mean~TargTempC_Avg.mean,data=thermo)
abline(lm2)
legend("topleft",legend=letters[2],cex=1.4,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

