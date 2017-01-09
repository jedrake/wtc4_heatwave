#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Plots the surface soil water content during the heatwave
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------



source("R/loadLibraries.R")


#-----------------------------------------------------------------------------------------------------------
#- download the soil volumetric water content data from HIEv, for WTC4

files <- searchHIEv("WTC_AUTO_C[0-9]{1,2}_SOILVARS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{1,2}_SOILVARS", startDate="2016-10-28", endDate="2016-11-11",
                  topath="C:/Repos/wtc4_heatwave/Data/fromHIEv",maxnfiles=100,
                  cachefile="C:/Repos/wtc4_heatwave/Data/fromHIEv/wtc4cache_VWC.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- as.factor(ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient"))
d$Source <- d$RECORD <- NULL
#d$VW_Avg <- rowMeans(d[,c("VW_Avg.1.","VW_Avg.2.","VW_Avg.3.")])

#- get the extra TDR data too
XTRAfiles <- searchHIEv("WTC_AUTO_C[0-9]{1,2}_XTRATDR")
d2 <- downloadTOA5("WTC_AUTO_C[0-9]{1,2}_XTRATDR", startDate="2016-10-28", endDate="2016-11-11",
                   topath="C:/Repos/wtc4_flux/data/fromHIEv",maxnfiles=100,
                   cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_XTRAVWC.rdata")
d2$chamber <- as.factor(substr(d2$Source,start=10,stop=12)) # extract the chamber number from the filename
d2$T_treatment <- as.factor(ifelse(as.numeric(substr(as.character(d2$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient"))
d2$Source <-d2$RECORD <- NULL

soildat <- merge(d,d2,by=c("Date","DateTime","chamber","T_treatment"),all.x=T)
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- Do some data manipulation
soildat$VW_surface <- rowMeans(soildat[,c("VW_Avg.1.","VW2_Avg.1.","VW2_Avg.2.")],na.rm=F)

#- remove all the bad soil temperature data...
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#calculate daily means, then treatment means
dfr.day <- summaryBy(.~Date+chamber+T_treatment,data=soildat,FUN=mean,keep.names=T,na.rm=T)


#- merge in the heatwave treatments
linkdf <- data.frame(chamber = levels(as.factor(soildat$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
dfr.day2 <- merge(dfr.day,linkdf,by="chamber")
dfr.day2$combotrt <- factor(paste(dfr.day2$T_treatment,dfr.day2$HWtrt,sep="_"))


dfr.day.treat <- summaryBy(.~Date+combotrt,data=subset(dfr.day2,Date>as.Date("2016-9-01")),
                           FUN=c(mean,se),keep.names=T,na.rm=T)
#dfr.day.a <- subset(dfr.day.treat,T_treatment=="ambient")
#dfr.day.e <- subset(dfr.day.treat,T_treatment=="elevated")


#- plot timecourse for treatments
windows(80,60);par(mfrow=c(2,1),cex.lab=1.5,mar=c(6,6,1,1))

palette(c("blue","black","orange","red"))

#- Single surface sensor
plotBy(VW_Avg.1..mean~Date|combotrt,data=dfr.day.treat,type="l",legend=F,ylim=c(0,0.3),
       ylab="VWC- single surface",lwd=3)
adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$VW_Avg.1..mean,SE=dfr.day.treat$VW_Avg.1..se,
             direction="updown",col=dfr.day.treat$combotrt,barlen=0)
#adderrorbars(x=dfr.day.e$Date,y=dfr.day.e$VW_Avg.1..mean,SE=dfr.day.e$VW_Avg.1..se,
#             direction="updown",col="red",barlen=0)
legend("top",lwd=3,lty=1,legend=levels(dfr.day$T_treatment),col=c("blue","red"),ncol=2)





windows(80,70);par(mfrow=c(3,1),cex.lab=1.75,cex.axis=1.5,mar=c(6,8,1,1),oma=c(0,2,4,0),las=1)
palette(c("blue","black","red","orange"))
ylims <- c(0,0.2)

#- average of three surface sensors
plotBy(VW_surface.mean~Date|combotrt,data=dfr.day.treat,type="l",legend=F,ylim=ylims,
       ylab="",lwd=3)
title(ylab="5-cm-depth",line=4)
#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-5"),format="%Y-%m-%d")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=15,col="darkgrey",density=7) #add rectangles for droughts

adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$VW_surface.mean,SE=dfr.day.treat$VW_surface.se,
             direction="updown",col=dfr.day.treat$combotrt,barlen=0)
plotBy(VW_surface.mean~Date|combotrt,data=dfr.day.treat,type="l",legend=F,ylim=c(0,0.3),add=T,
       ylab="",lwd=3)
legend(x=as.Date("2016-10-30"),y=0.3,xpd=NA,lwd=3,col=palette()[1:4],ncol=2,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")
abline(h=c(0.05,0.2),lty=2)

#- mid depth
plotBy(VW_Avg.2..mean~Date|combotrt,data=dfr.day.treat,type="l",legend=F,ylim=ylims,
       ylab="",lwd=3)
adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$VW_Avg.2..mean,SE=dfr.day.treat$VW_Avg.2..se,
             direction="updown",col=dfr.day.treat$combotrt,barlen=0)
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=15,col="darkgrey",density=7) #add rectangles for droughts
title(ylab="30-cm-depth",line=4)
legend("topright",legend=letters[2],cex=2,bty="n")
abline(h=c(0.05,0.2),lty=2)

#- deep
plotBy(VW_Avg.3..mean~Date|combotrt,data=dfr.day.treat,type="l",legend=F,ylim=ylims,
       ylab="",lwd=3)
adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$VW_Avg.3..mean,SE=dfr.day.treat$VW_Avg.3..se,
             direction="updown",col=dfr.day.treat$combotrt,barlen=0)
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=15,col="darkgrey",density=7) #add rectangles for droughts
title(ylab="90-cm-depth",line=4)
legend("topright",legend=letters[3],cex=2,bty="n")
abline(h=c(0.05,0.2),lty=2)


title(ylab=expression(VWC~(m^3~m^-3)),outer=T,cex.lab=3,line=-2)
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- average VWC during the heatwave
vwc.hw <- subset(dfr.day,Date >= as.Date("2016-10-31") & Date <= as.Date("2016-11-4"))
vwc.hw$VWCall <- rowMeans(vwc.hw[,c("VW_surface","VW_Avg.2.","VW_Avg.3.")])

mean(vwc.hw$VWCall)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------