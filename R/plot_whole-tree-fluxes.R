#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Plot the whole-tree flux data during the heatwave
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------


source("R/loadLibraries.R")



#-----------------------------------------------------------------------------------------------------------
#- first, read in the canopy mass and SLA data, to estimate total tree leaf area at harvest
harvest <- read.csv("Data/Harvest/WTC_TEMP_CM_PARRA_CANOPY-HARVEST_20161121_L0.csv")
harvest$LeafArea <- with(harvest,Leaf_DW*SLA/10000) # calculate total leaf area (m2) for each canopy layer
harvest.sum <- summaryBy(LeafArea~chamber,data=harvest,FUN=sum,keep.names=T) # sum across the three canopy layers
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- Read in and process the flux data associated with the heatwave. Calculate leaf-scale fluxes.

wtc1 <- read.csv("Data/WTC_TEMP-PARRA_WTCFLUX_20161028-20161115_L0.csv")

wtc1$DateTime <- as.POSIXct(wtc1$DateTime,format="%Y-%m-%d %T",tz="GMT")
wtc1$VPD <- RHtoVPD(RH=wtc1$RH_al,TdegC=wtc1$Tair_al)

starttime <- as.POSIXct("2016-10-29 00:00:00",tz="GMT")
wtc <- subset(wtc1,DateTime>starttime & DoorCnt == 0)

#-- create hourly values for subsequent averaging of fluxes
wtc$DateTime_hr <- nearestTimeStep(wtc$DateTime,nminutes=60,align="floor")

#- NA-fill some crap CO2 flux data
tonafill <- which(wtc$FluxCO2 > 0.5 | wtc$FluxCO2 < -1) # a few obvious outliers
wtc$FluxCO2[tonafill] <- NA
tonafill2 <- which(as.Date(wtc$DateTime_hr)==as.Date("2016-11-07") & wtc$PAR>1200 & wtc$FluxCO2 < 0.05) # some bad data when controlling temperatures
wtc$FluxCO2[tonafill2] <- NA
wtc$FluxH2O[tonafill2] <- NA

#- average PAR, VPD, FLuxCO2 and FLuxH2O for plotting
wtc.m1 <- summaryBy(PAR+VPD+Tair_al+FluxCO2+FluxH2O~DateTime_hr+chamber+T_treatment,data=subset(wtc,DoorCnt==0),
                   FUN=mean,keep.names=T,na.rm=T)


#- merge in the heatwave treatments and total leaf area data
linkdf <- data.frame(chamber = levels(as.factor(wtc$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08

wtc.m1 <- merge(wtc.m1,linkdf,by="chamber")
wtc.m <- merge(wtc.m1,harvest.sum,by="chamber")
wtc.m$combotrt <- factor(paste(wtc.m$T_treatment,wtc.m$HWtrt,sep="_"))

#- calculate leaf-area specific photosynthetic and transpiration rates
wtc.m$Photo <- with(wtc.m,FluxCO2*1000/LeafArea) # convert to units of umol CO2 m-2 s-1
wtc.m$Trans <- with(wtc.m,FluxH2O*1000/LeafArea) # convert to units of mmol H2O m-2 s-1

#- average and SEs for each treatment. Originally ended on 2016-11-06
wtc.m2 <- summaryBy(.~DateTime_hr+combotrt,data=subset(wtc.m,as.Date(DateTime_hr) < as.Date("2016-11-11")),FUN=c(mean,se))
#-----------------------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot Time courses of Photosynthesis and Transpiraiton during the heatwave.

windows(80,60)
par(mfrow=c(2,1),mar=c(2,6,1,6),oma=c(0,0,4,0),cex.lab=1.6)
palette(c("blue","black","orange","red"))
# 
# 
# #- plot PAR and VPD
# plot(PAR.mean~DateTime_hr,data=wtc.m2,type="l",lwd=2,lty=2,col="grey",ylab="PPFD")
# par(new = T)
# plotBy(Tair_al.mean~DateTime_hr|combotrt, data=wtc.m2,type="l",lwd=2, axes=F, xlab=NA, ylab=NA,
#        ylim=c(5,45),legend=F)
# axis(side = 4,ylab="Tair",col="red",col.axis="red")
#legend(x=starttime-10000,y=64,xpd=NA,legend=levels(wtc.m2$combotrt),lwd=2,col=palette()[1:4],ncol=2,cex=1.5,bty="n")
# title(ylab="Tair",col="red",xpd=NA,line=-50,col="red")
# 
# plotBy(VPD.mean~DateTime_hr|combotrt, data=wtc.m2,type="l",lwd=2, legend=F,ylab="VPD",
#        ylim=c(0,6))
# axis(side = 4,ylab="",col="black",col.axis="black")

#- plot photosynthesis
plotBy(Photo.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(-1,12),las=1,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0)


#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 16:00","2016-11-4 10:00"),format="%Y-%m-%d %R")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=15,col="darkgrey",density=7) #add rectangles for droughts

#- replot and add error bars
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$Photo.mean,SE=wtc.m2$Photo.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
plotBy(Photo.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3.5,ylim=c(-1,12),las=1,add=T,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))
legend(x=starttime-42000,y=17,xpd=NA,lwd=3,col=palette()[1:4],ncol=2,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")


#- plot transpiration
plotBy(Trans.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(0,3),las=1,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-1,xright=dates[2],ytop=4,col="darkgrey",density=7) #add rectangles for droughts

axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$Trans.mean,SE=wtc.m2$Trans.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
plotBy(Trans.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3.5,ylim=c(0,3),las=1,add=T,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
legend("topright",legend=letters[2],cex=2,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the average  Acanopy and Ecanopy during the hot afternoon during the heatwave for each treatment.
hw1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3"))
hw <- subset(hw1,hour(DateTime_hr) >= 12 & hour(DateTime_hr) <= 14)

boxplot(Photo~combotrt,data=hw)
summaryBy(Photo~HWtrt,data=hw)
summaryBy(Trans~HWtrt,data=hw)

#- do the same, but for the three days after the heatwave
post1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-11-4") & as.Date(DateTime_hr) <= as.Date("2016-11-6"))
post <- subset(post1,hour(DateTime_hr) >= 10 & hour(DateTime_hr) <= 16)

boxplot(Photo~combotrt,data=post)
summaryBy(Photo~HWtrt,data=post)
summaryBy(Trans~HWtrt,data=post)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot the divergence between CO2 uptake and H2O release

#- subset to relatively high light, calculate WUEi
wtc.m.highpar <- subset(wtc.m,PAR>800 & as.Date(DateTime_hr) >= as.Date("2016-10-30") & as.Date(DateTime_hr) <= as.Date("2016-11-11"))

#- NA-fill some outliers
tonafill1 <- which(wtc.m.highpar$WUEi>40)
tonafill2 <- which(wtc.m.highpar$WUEi>15 & wtc.m.highpar$VPD>2)
tonafill3 <- which(wtc.m.highpar$WUEi>5 & wtc.m.highpar$VPD>4)
wtc.m.highpar$FluxCO2[c(tonafill1,tonafill2,tonafill3)] <- NA
wtc.m.highpar$FluxH2O[c(tonafill1,tonafill2,tonafill3)] <- NA

wtc.m.highpar$WUEi <- with(wtc.m.highpar,FluxCO2/FluxH2O)

windows(60,80)
par(mfrow=c(3,1),mar=c(2,6,1,4),oma=c(4,0,4,0),cex.lab=1.5,cex.axis=1.2,las=1)
plotBy(Photo~VPD|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0);axis(4)
legend(x=1,y=17,xpd=NA,pch=16,col=palette()[1:4],ncol=2,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")

plotBy(Trans~VPD|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));axis(4)
legend("topright",legend=letters[2],cex=2,bty="n")

plotBy(WUEi~VPD|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(A[canopy]~"/"~E[canopy]));abline(h=0);axis(4)
title(xlab=expression(VPD~(kPa)),outer=T,line=2,cex.lab=2)
legend("topright",legend=letters[3],cex=2,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




