
source("R/loadLibraries.R")
library(RColorBrewer)
library(scales)
library(colorRamps)
library(plantecophys)
library(RODBC)
library(lubridate)
library(dplyr)

#-----------------------------------------------------------------------------------------------------------
#- download the leaf temperature data from HIEv (i.e., the actual thermocouples!)
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_LEAFTEMPS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{2}_LEAFTEMPS", startDate="2016-10-28", endDate="2016-11-30",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_Tleaf.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
d$DateTime_hr <- nearestTimeStep(d$DateTime, nminutes = 15, align = "floor")

#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- download the AIRVARS data (PPFD and IR-T data), do some manipulation
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_AIRVARS")
IRT <- downloadTOA5("WTC_AUTO_C[0-9]{2}_AIRVARS", startDate="2016-10-28", endDate="2016-11-30",
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

# max(TrendlogChDF$DateTime)
# startDate<- as.Date("2016-10-30")
# endDate<- as.Date("2016-10-30")
# dates<-seq(as.Date(startDate),as.Date(endDate),by="day")
# #get relevant trendlog data for each day and append to Ref and Chamber data files
# for (i in dates){
#   d2<-as.Date(i,origin="1970-01-01")
#   message("getting ",d2)
#   dat<-getWTCtrendlog(d2,timestep=1)
#   TrendlogChDF<-rbind(TrendlogChDF,dat[[1]])
#   TrendlogRefDF<-rbind(TrendlogRefDF,dat[[2]])
# }
# TrendlogChDF<-TrendlogChDF[!duplicated(TrendlogChDF[1:2]),] #remove any duplicated data rows
# TrendlogRefDF<-TrendlogRefDF[!duplicated(TrendlogRefDF[1]),] #remove any duplicated data rows
# 
# #resave datafiles
# save(TrendlogChDF,file="C:/Repos/wtc4_flux_processing/data/TrendlogChDF.RData")
# save(TrendlogRefDF,file="C:/Repos/wtc4_flux_processing/data/TrendlogRefDF.RData")


Trend.dat <- subset(TrendlogChDF,as.Date(DateTime)>=as.Date("2016-10-28"))
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
dat <- merge(dat2,Trend.dat.m,by=c("chamber","DateTime_hr"))

linkdf <- data.frame(chamber = levels(as.factor(dat$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
dat <- merge(dat,linkdf,by="chamber")
dat$combotrt <- factor(paste(dat$T_treatment,dat$HWtrt,sep="_"))
dat$Tleaf <- rowMeans(dat[,c("LeafT_Avg.1.","LeafT_Avg.2.")],na.rm=T)

dat$Tleafmean <- rowMeans(dat[,c("Tleaf","TargTempC_Avg")])

dat <- subset(dat,as.Date(DateTime_hr)>=as.Date("2016-10-29") & as.Date(DateTime_hr)<=as.Date("2016-11-6"))

#- average across treatments
dat.m <- summaryBy(.~DateTime_hr+T_treatment+HWtrt,data=dat,FUN=c(mean,se),keep.names=T)
dat.m$combotrt <- factor(paste(dat.m$T_treatment,dat.m$HWtrt,sep="_"))

#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#- plots
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------

#- plot air and leaf temperatures
windows(60,60)
par(mfrow=c(3,1),mar=c(2,6,1,6),oma=c(4,0,1,0),cex.lab=1.6)
palette(c("blue","black","red","orange"))


#- plot PAR, VPD, and temperatures
plotBy(PPFD_Avg.mean~DateTime_hr,data=subset(dat.m,combotrt=="ambient_C"),legend=F,col="black",type="l",lwd=2,ylim=c(0,2000),ylab="PPFD")
axis(side=4)

#- thermocouples
plotBy(LeafT_Avg.1.~DateTime_hr|combotrt,data=dat,
       type="p",ylab="Leaf T (thermocouples)",ylim=c(5,50),legend=F)
plotBy(LeafT_Avg.2.~DateTime_hr|combotrt,data=dat,
       type="p",ylab="Leaf T (deg C)",add=T,legend=F)
plotBy(Tair_al.mean~DateTime_hr|combotrt,data=dat.m,type="l",add=T,lty=1,lwd=3,legend=F)
legend("topleft",xpd=NA,legend=paste("Tleaf",levels(dat.m$combotrt)),pch=1,col=palette()[1:4],ncol=1,cex=1.3,bty="n")
legend("bottomleft",xpd=NA,legend=paste("Tair",levels(dat.m$combotrt)),lty=1,lwd=3,col=palette()[1:4],ncol=2,cex=1.5,bty="n")

axis(side=4)

#- IR temperatures
plotBy(TargTempC_Avg~DateTime_hr|combotrt,data=dat,
       type="p",ylab="Leaf T (IR)",ylim=c(5,50),legend=F)
plotBy(Tair_al.mean~DateTime_hr|combotrt,data=dat.m,type="l",add=T,lty=1,lwd=3,legend=F)
axis(side=4)
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- plot IR leaf temperatures again, relative to air temperature


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




# 
# #-----------
# #- air and leaf temperatures in ambient
# plotBy(Tair_al.mean~DateTime_hr|combotrt, data=dat.m,type="l",lty=3,lwd=2,ylim=c(10,45),legend=F,ylab="T (deg C)")
# plotBy(Tleafmean.mean~DateTime_hr|combotrt, data=dat.m,type="l",add=T,lty=1,lwd=3,legend=F)
# adderrorbars(x=dat.m$DateTime_hr,y=dat.m$Tleafmean.mean,SE=dat.m$Tleafmean.se,direction="updown",col=dat.m$combotrt,barlen=0)
# axis(side=4)
# #plotBy(TargTempC_Avg~DateTime_hr|combotrt, data=subset(dat.m,T_treatment=="ambient"),type="l",add=T,lty=2,lwd=2,legend=F)
# legend("topleft",c("Air","Leaf"),lty=c(3,1),lwd=c(2,3),seg.len=6)
# ##----------------------------------------------------------------------------------------------------------
# 
# 
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# #- plot a focal day
# toplot.all <- subset(dat,as.Date(DateTime_hr)==as.Date("2016-11-03"))
# toplot.l <- split(toplot.all,toplot.all$chamber)
# 
# pdf(file="output/Tleaf_comparison_2016-11-03_HW.pdf")
# par(mfrow=c(2,2))
# for(i in 1:length(toplot.l)){
#   toplot <- toplot.l[[i]]
#   
#   #- loop over each chamber, print a pdf
#   #palette(c("black","forestgreen",brewer.pal(4,"Spectral")))
#   palette(blue2green2red(12))
#   
#   #- plot PAR
#   plotBy(PPFD_Avg~DateTime_hr,data=toplot,legend=F,legendwhere="topright",type="l",ylim=c(0,1500),col="black",
#          ylab="PAR")
#   title(main=paste(toplot$chamber[1],as.Date(toplot$DateTime)[1],toplot$combotrt[1],sep=" "))
#   
#   
#   #- plot Temperatures
#   plotBy(TargTempC_Avg~Tleaf|chamber,data=toplot,legendwhere="topleft",type="p",xlim=c(5,45),ylim=c(5,45),pch=1,
#          legend=F,col="black",
#          xlab=expression(T[leaf]~(avg~of~2~thermocouples)),ylab=expression(T[leaf]~(IR)))
#   abline(0,1)
#   
#   #- plot temperatures
#   plotBy(TargTempC_Avg~DateTime_hr|chamber,data=toplot,legend=F,legendwhere="topright",type="l",ylim=c(5,45),lwd=2,col="black",
#          ylab=expression("T"~(degree*C)))
#   plotBy(Tleaf~DateTime_hr|chamber,data=toplot,legend=F,legendwhere="topright",type="l",lty=2,add=T,col="black",
#          ylab="Tleaf")
#   plotBy(Tair_al~DateTime_hr|chamber,data=toplot,legendwhere="topright",type="l",add=T,lty=1,lwd=2,col="grey",legend=F)
#   legend("topleft",lty=c(1,2,1),bty="n",legend=c("IR","TC","Tair"),col=c("black","black","grey"))
#   
#   #- plot Tdiff vs. PAR
#   toplot$Tdiff_IR <- with(toplot,TargTempC_Avg-Tair_al)
#   toplot$Tdiff_TC <- with(toplot,Tleaf-Tair_al)
#   plotBy(Tdiff_IR~PPFD_Avg|chamber,data=toplot,legend=F,type="p",xlim=c(0,1500),ylim=c(-1,6),pch=16,col="black",
#          xlab=expression(PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(T[leaf]-T[air]~(degree*C)))
#   plotBy(Tdiff_TC~PPFD_Avg|chamber,data=toplot,legend=F,type="p",xlim=c(0,1500),pch=1,add=T,col="black",
#          xlab=expression(PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(T[leaf]-T[air]~(degree*C)))
#   abline(h=0)
#   legend("topleft",pch=c(1,16),legend=c("Thermocouples","IR sensor"),col="black",bty="n")
# }
# dev.off()
