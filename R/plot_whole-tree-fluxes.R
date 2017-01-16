#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Plot the whole-tree flux data during the heatwave
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------


source("R/loadLibraries.R")


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- first, read in the canopy mass and SLA data, to estimate total tree leaf area at harvest
harvest <- read.csv("Data/Harvest/WTC_TEMP_CM_PARRA_CANOPY-HARVEST_20161121_L0.csv")
harvest$LeafArea <- with(harvest,Leaf_DW*SLA/10000) # calculate total leaf area (m2) for each canopy layer
harvest.sum <- summaryBy(LeafArea~chamber,data=harvest,FUN=sum,keep.names=T) # sum across the three canopy layers
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






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
wtc$FluxH2O[tonafill] <- NA

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
#-----------------------------------------------------------------------------------------------------------
#- plot time courses of air temperature, photosynthesis and transpiraiton during the heatwave.

windows(80,70)
par(mar=c(2,6,1,6),oma=c(2,0,4,0),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,2,2))
palette(c("blue","black","orange","red"))

#---
#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|combotrt,data=wtc.m2,type="l",lwd=3,lty=1,
       ylab=expression(T[air]~(degree*C)),ylim=c(5,45),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$Tair_al.mean,SE=wtc.m2$Tair_al.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
plotBy(Tair_al.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3, add=T)

#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 18:00","2016-11-4 16:00"),format="%Y-%m-%d %R")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=55,col="darkgrey",density=7) 
#- add legend
legend(x=starttime-112000,y=65,xpd=NA,lwd=3,col=palette()[1:4],ncol=4,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")

#---
#- plot photosynthesis
plotBy(Photo.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(-1,12),las=1,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-5,xright=dates[2],ytop=20,col="darkgrey",density=7) #add rectangles for heatwave

#- replot and add error bars
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$Photo.mean,SE=wtc.m2$Photo.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
plotBy(Photo.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3.5,ylim=c(-1,12),las=1,add=T,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))

legend("topright",legend=letters[2],cex=2,bty="n")

#---
#- plot transpiration
plotBy(Trans.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(0,3),las=1,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-1,xright=dates[2],ytop=4,col="darkgrey",density=7) #add rectangles for heatwave

axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$Trans.mean,SE=wtc.m2$Trans.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
plotBy(Trans.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3.5,ylim=c(0,3),las=1,add=T,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
legend("topright",legend=letters[3],cex=2,bty="n")


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot time courses of air temperature, photosynthesis and transpiraiton during the heatwave.
#- ALTERNATE VERSION WITH TWO TREATMENTS

wtc.m2.two <- summaryBy(.~DateTime_hr+HWtrt,
                        data=subset(wtc.m,as.Date(DateTime_hr) <= as.Date("2016-11-6") & as.Date(DateTime_hr) >= as.Date("2016-10-30")),FUN=c(mean,se))


windows(80,70)
par(mar=c(2,6,1,6),oma=c(2,0,4,0),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,2,2))
palette(c("blue","red"))

#---
#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|HWtrt,data=wtc.m2.two,type="l",lwd=3,lty=1,
       ylab=expression(T[air]~(degree*C)),ylim=c(5,45),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Tair_al.mean,SE=wtc.m2.two$Tair_al.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)

#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 18:00","2016-11-4 16:00"),format="%Y-%m-%d %R")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=55,col="darkgrey",density=7) 
#- add legend
legend(x=starttime+212000,y=65,xpd=NA,lwd=3,col=palette()[1:2],ncol=4,cex=1.5,bty="n",
       legend=c("Control","Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")

#---
#- plot photosynthesis
plotBy(Photo.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3,ylim=c(-1,12),las=1,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-5,xright=dates[2],ytop=20,col="darkgrey",density=7) #add rectangles for heatwave

#- replot and add error bars
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Photo.mean,SE=wtc.m2.two$Photo.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)
plotBy(Photo.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3.5,ylim=c(-1,12),las=1,add=T,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))

legend("topright",legend=letters[2],cex=2,bty="n")

#---
#- plot transpiration
plotBy(Trans.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3,ylim=c(0,3),las=1,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-1,xright=dates[2],ytop=4,col="darkgrey",density=7) #add rectangles for heatwave

axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Trans.mean,SE=wtc.m2.two$Trans.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)
plotBy(Trans.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3.5,ylim=c(0,3),las=1,add=T,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
legend("topright",legend=letters[3],cex=2,bty="n")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------












# 
# 
# 
# 
# 
# 
# 
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------
# #- plot respiration during the heatwave
# 
# #- get just the respiration data
# wtc.R1 <- subset(wtc.m,PAR<4)
# wtc.R <- subset(wtc.R1, hour(wtc.m$DateTime_hr) >22 | hour(wtc.m$DateTime_hr) < 4)
# wtc.R$Respiration <- -1*wtc.R$Photo
# 
# #- exclude the data in the late evening of 2016-11-10 that looks bad
# tonafill <- which(as.Date(wtc.R$DateTime_hr) == as.Date("2016-11-10") & hour(wtc.R$DateTime_hr) >=22)
# wtc.R$Respiration[tonafill] <- NA
# 
# 
# 
# 
# 
# #----
# #- plot R vs. T
# windows(80,60)
# par(mfrow=c(2,1),mar=c(2,6,1,6),oma=c(4,0,4,0),cex.lab=1.6)
# palette(c("blue","black","orange","red"))
# 
# #- plot respiration
# plotBy(Respiration~Tair_al|combotrt,data=subset(wtc.R,as.Date(DateTime_hr)<=as.Date("2016-11-04")),legend=F,type="p",pch=16,ylim=c(0,2),las=1,
#        ylab=expression(R[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))
# legend("bottomright","During heat wave",bty="n")
# legend(x=7,y=2.8,xpd=NA,pch=16,col=palette()[1:4],ncol=2,cex=1.5,bty="n",
#        legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
# 
# 
# plotBy(Respiration~Tair_al|combotrt,data=subset(wtc.R,as.Date(DateTime_hr)>as.Date("2016-11-04")),legend=F,type="p",pch=16,ylim=c(0,2),las=1,
#        ylab=expression(R[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))
# legend("bottomright","After heat wave",bty="n")
# title(xlab=expression(T[air]~(degree*C)),outer=T,line=2)
# #----
# 
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# #--- 
# #- plot time courses of respiration and temperature
# #- average and SEs for each treatment. Originally ended on 2016-11-06
# wtc.Rm2 <- summaryBy(.~DateTime_hr+combotrt,data=subset(wtc.R,as.Date(DateTime_hr) < as.Date("2016-11-11")),FUN=c(mean,se))
# 
# windows(80,60)
# par(mfrow=c(2,1),mar=c(2,6,1,6),oma=c(0,0,4,0),cex.lab=1.6)
# palette(c("blue","black","orange","red"))
# #- plot Respiration
# plotBy(Respiration.mean~DateTime_hr|combotrt,data=wtc.Rm2,legend=F,type="p",pch=16,ylim=c(0,2),las=1,
#        ylab=expression(R[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0)
# 
# #- add shaded rectangle for heatwave
# dates <- as.POSIXct(c("2016-10-31 16:00","2016-11-4 10:00"),format="%Y-%m-%d %R")
# rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=15,col="darkgrey",density=7) #add rectangles for droughts
# 
# #- replot and add error bars
# axis(side = 4,ylab="",col="black",col.axis="black",las=1)
# adderrorbars(x=wtc.Rm2$DateTime_hr,y=wtc.Rm2$Respiration.mean,SE=wtc.Rm2$Respiration.se,
#              direction="updown",col=wtc.Rm2$combotrt,barlen=0,lwd=0.5)
# plotBy(Respiration.mean~DateTime_hr|combotrt,data=wtc.Rm2,legend=F,type="p",pch=16,cex=1.2,ylim=c(-1,12),las=1,add=T)
# legend(x=starttime-42000,y=2.75,xpd=NA,pch=16,col=palette()[1:4],ncol=2,cex=1.5,bty="n",
#        legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
# 
# #- plot temperatures
# plotBy(Tair_al.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(5,50),las=1,
#        ylab=expression(T[air]~(degree*C)));abline(h=0)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



















#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the average  Acanopy and Ecanopy during the hot afternoon during the heatwave for each treatment.
hw1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3"))
hw <- subset(hw1,hour(DateTime_hr) >= 12 & hour(DateTime_hr) <= 14)
hw$RH <- VPDtoRH(VPD=hw$VPD,TdegC=hw$Tair_al)

boxplot(Photo~combotrt,data=hw)
summaryBy(Photo~HWtrt,data=hw)
summaryBy(Trans~HWtrt,data=hw)
summaryBy(RH~HWtrt,data=hw)
summaryBy(VPD~HWtrt,data=hw)

#- do the same, but for the three days after the heatwave
post1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-11-4") & as.Date(DateTime_hr) <= as.Date("2016-11-6"))
post <- subset(post1,hour(DateTime_hr) >= 10 & hour(DateTime_hr) <= 16)

boxplot(Photo~combotrt,data=post)
summaryBy(Photo~HWtrt,data=post)
summaryBy(Trans~HWtrt,data=post)

#- calculate the average Ecanopy during the night during the heatwave for each treatment.
hw.night1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3") & PAR < 4)
hw.night <- subset(hw.night1,hour(DateTime_hr) <= 4 | hour(DateTime_hr) >= 22)

boxplot(Trans~combotrt,data=hw.night)
summaryBy(Photo~HWtrt,data=hw.night)
summaryBy(Trans~HWtrt,data=hw.night)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot the divergence between CO2 uptake and H2O release

#- subset to relatively high light, calculate WUEi
wtc.m.highpar <- subset(wtc.m,PAR>800 & as.Date(DateTime_hr) >= as.Date("2016-10-30") & as.Date(DateTime_hr) <= as.Date("2016-11-11"))
wtc.m.highpar$WUEi <- with(wtc.m.highpar,FluxCO2/FluxH2O)

#- NA-fill some outliers
tonafill1 <- which(wtc.m.highpar$WUEi>40)
tonafill2 <- which(wtc.m.highpar$WUEi>15 & wtc.m.highpar$VPD>2)
tonafill3 <- which(wtc.m.highpar$WUEi>5 & wtc.m.highpar$VPD>4)
wtc.m.highpar$FluxCO2[c(tonafill1,tonafill2,tonafill3)] <- NA
wtc.m.highpar$FluxH2O[c(tonafill1,tonafill2,tonafill3)] <- NA
wtc.m.highpar$WUEi <- with(wtc.m.highpar,FluxCO2/FluxH2O)


#- correlation between vpd and Tair
lm.vpd <- lm(VPD~Tair_al,data=wtc.m.highpar)
summary(lm.vpd)

#----
#- predict what fluxes SHOULD have been
Tvals <- seq(18,45, length.out=101)
VPDs <- predict(lm.vpd,newdata=data.frame(Tair_al=Tvals))
predicts <- Photosyn(VPD=VPDs,Tleaf=Tvals)
#----

windows(120,80)
par(mfrow=c(3,3),mar=c(2,6,1,4),oma=c(4,0,4,0),cex.lab=1.5,cex.axis=1.2,las=1)

#- photo vs. Tair
plotBy(Photo~Tair_al|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0);axis(4)
legend(x=20,y=17,xpd=NA,pch=16,col=palette()[1:4],ncol=4,cex=1.5,bty="n",
       legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")

# photo vs. VPD
plotBy(Photo~VPD|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0);axis(4)
legend("topright",legend=letters[4],cex=2,bty="n")

#---
#- Plot model predictions
plot(ALEAF~VPD,data=predicts,legend=F,pch=16,type="o",lwd=2,
     ylab=expression(Modeled~A[leaf]));abline(h=0);axis(4)
legend("topright",legend=letters[7],cex=2,bty="n")
title(main="Modeled")

#Trans vs. Tair
plotBy(Trans~Tair_al|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));axis(4)
legend("topright",legend=letters[2],cex=2,bty="n")

#- trans vs. VPD
plotBy(Trans~VPD|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));axis(4)
legend("topright",legend=letters[5],cex=2,bty="n")

#- Plot model predictions
plot(ELEAF~VPD,data=predicts,legend=F,pch=16,type="o",lwd=2,
     ylab=expression(Modeled~E[leaf]));abline(h=0);axis(4)
legend("topright",legend=letters[8],cex=2,bty="n")

#- wuei vs. Tair
plotBy(WUEi~Tair_al|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(A[canopy]~"/"~E[canopy]));abline(h=0);axis(4)
legend("topright",legend=letters[3],cex=2,bty="n")

#- wuei vs. VPD
plotBy(WUEi~VPD|combotrt,data=wtc.m.highpar,legend=F,pch=16,
       ylab=expression(A[canopy]~"/"~E[canopy]));abline(h=0);axis(4)
legend("topright",legend=letters[6],cex=2,bty="n")


#- Plot model predictions
plot(ALEAF/ELEAF~VPD,data=predicts,legend=F,pch=16,type="o",lwd=2,ylim=c(0,20),
     ylab=expression(Modeled~A[leaf]~"/"~E[leaf]));abline(h=0);axis(4)
legend("topright",legend=letters[9],cex=2,bty="n")


title(xlab=expression(T[air]~(degree*C)),outer=T,line=2,cex.lab=2,adj=0.15)
title(xlab=expression(VPD~(kPa)),outer=T,line=2,cex.lab=2,adj=0.5)
title(xlab=expression(VPD~(kPa)),outer=T,line=2,cex.lab=2,adj=0.87)


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- sum Photo and Trans fluxes throughout the heatwave and four following days

#- subset to the desired timeframe, convert to g C and kg H2O hr-1
tosum <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-7"))
tosum$Photo.g <- tosum$Photo*1e-6*12.011*60*60
tosum$Trans.kg <- tosum$Trans*1e-3*18.015*60*60/1000



###--- The entire 8-day period
#- sum across chambers. Note that there are a few NA's, which will modestly affect the sums.
tosum.c <- summaryBy(Photo.g+Trans.kg~chamber+T_treatment+HWtrt+combotrt,data=tosum,FUN=sum,keep.names=T,na.rm=T)
boxplot(Photo.g~HWtrt+T_treatment,data=tosum.c) # reduction with heatwave
boxplot(Trans.kg~HWtrt+T_treatment,data=tosum.c) # no change

#- calculate mean and SE of chamber sums, make grouped bar chart
sums.m <- summaryBy(Photo.g+Trans.kg~combotrt,data=tosum.c,FUN=c(mean,se))


#- pull out the means to plot
A_c <- sums.m[which(sums.m$combotrt=="ambient_C"),]
A_HW <- sums.m[which(sums.m$combotrt=="ambient_HW"),]
E_c <- sums.m[which(sums.m$combotrt=="elevated_C"),]
E_HW <- sums.m[which(sums.m$combotrt=="elevated_HW"),]

toplot   <- as.matrix(rbind(A_c[,2:3], A_HW[,2:3], E_c[,2:3], E_HW[,2:3]))
rownames(toplot) <- c("A_c", "A_HW", "E_c","E_HW")
colnames(toplot) <- c("Photo","Trans")
###--- 

windows()
par(oma=c(2,4,0,0))
palette(c("blue","darkgrey","orange","red"))

xvals <- barplot(toplot, beside=T, ylab="", names.arg=c("",""),
        cex.names=1.5, las=1, ylim=c(0,30), col=c("blue","darkgrey","orange","red"))
adderrorbars(x=xvals[,1],y=toplot[,1],SE=sums.m$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,2],y=toplot[,2],SE=sums.m$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
box(bty="l")
graphics::text(x=c(xvals[3,1]-0.5,xvals[3,2]-0.5),y=-5,xpd=NA,labels=c("Photosynthesis","Transpiration"),cex=1.8) # x-axis labels
title(ylab=expression(Total~Flux~(g~C~m^-2*";"~kg~H[2]*O~m^-2)),cex.lab=1.8,xpd=NA) # y-axis label
legend("topleft",legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"),
       fill=palette()[1:4],ncol=2,bty="n",cex=1.2)

###--- 
#-- add just the fluxes from during the heatwave
tosum.HW <- summaryBy(Photo.g+Trans.kg~chamber+T_treatment+HWtrt+combotrt,
                      data=subset(tosum,as.Date(DateTime_hr)<=as.Date("2016-11-04")),FUN=sum,keep.names=T,na.rm=T)
sums.HW <- summaryBy(Photo.g+Trans.kg~combotrt,data=tosum.HW,FUN=c(mean,se))

#- pull out the means to plot
A_c2 <- sums.HW[which(sums.HW$combotrt=="ambient_C"),]
A_HW2 <- sums.HW[which(sums.HW$combotrt=="ambient_HW"),]
E_c2 <- sums.HW[which(sums.HW$combotrt=="elevated_C"),]
E_HW2 <- sums.HW[which(sums.HW$combotrt=="elevated_HW"),]

toplot2  <- as.matrix(rbind(A_c2[,2:3], A_HW2[,2:3], E_c2[,2:3], E_HW2[,2:3]))
rownames(toplot2) <- c("A_c", "A_HW", "E_c","E_HW")
colnames(toplot2) <- c("Photo_HW","Trans_HW")
###--- 

barplot(toplot2, beside=T, ylab="", names.arg=c("",""),add=T,density=40,
                 cex.names=1.5, las=1, ylim=c(0,45), col="black")
adderrorbars(x=xvals[,1],y=toplot2[,1],SE=sums.HW$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,2],y=toplot2[,2],SE=sums.HW$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)



###--- 
#-- add just the fluxes from after the heatwave
tosum.REC <- summaryBy(Photo.g+Trans.kg~chamber+T_treatment+HWtrt+combotrt,
                      data=subset(tosum,as.Date(DateTime_hr)>as.Date("2016-11-04")),FUN=sum,keep.names=T,na.rm=T)
sums.REC <- summaryBy(Photo.g+Trans.kg~combotrt,data=tosum.REC,FUN=c(mean,se))

#- pull out the means to plot
A_c3 <- sums.REC[which(sums.REC$combotrt=="ambient_C"),]
A_HW3 <- sums.REC[which(sums.REC$combotrt=="ambient_HW"),]
E_c3 <- sums.REC[which(sums.REC$combotrt=="elevated_C"),]
E_HW3 <- sums.REC[which(sums.REC$combotrt=="elevated_HW"),]

toplot3  <- as.matrix(rbind(A_c3[,2:3], A_HW3[,2:3], E_c3[,2:3], E_HW3[,2:3]))
rownames(toplot3) <- c("A_c", "A_HW", "E_c","E_HW")
colnames(toplot3) <- c("Photo_post","Trans_post")
###--- 





###---- Plot the two periods separately

toplot.all3 <- cbind(toplot2[,1],toplot3[,1],toplot2[,2],toplot3[,2])

windows()
par(oma=c(3,4,0,0))
palette(c("blue","darkgrey","orange","red"))

#- plot flux sums for CO2 (first 2 groups, HW, REC) and then H2O (last 2 groups, HW, REC)
xvals <- barplot(toplot.all3, beside=T, ylab="", names.arg=rep("",ncol(toplot.all3)),
                 cex.names=1.5, las=1, ylim=c(0,16), col=c("blue","darkgrey","orange","red"))
adderrorbars(x=xvals[,1],y=toplot.all3[,1],SE=sums.HW$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,2],y=toplot.all3[,2],SE=sums.REC$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,3],y=toplot.all3[,3],SE=sums.HW$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,4],y=toplot.all3[,4],SE=sums.REC$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
abline(v=10.5)


box(bty="l")
graphics::text(x=c(5.5,16),y=16,xpd=NA,labels=c("Photosynthesis","Transpiration"),cex=1.8) # x-axis labels
title(ylab=expression(Total~Flux~(g~C~m^-2*";"~kg~H[2]*O~m^-2)),cex.lab=1.8,xpd=NA) # y-axis label
legend(x=11,y=13,legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"),
       fill=palette()[1:4],ncol=1,bty="b",cex=1.2)
text(x=xvals[3,]+.5,y=-1,labels=rep(c("Heatwave","Recovery"),2),xpd=T,srt=45,pos=2,cex=1.4)
###---- 






#- calculate % change in response to heatwave for photo and trans
#  over teh eight day period (4 heatwave days + 4 recovery days)
sums.m2 <- summaryBy(Photo.g+Trans.kg~HWtrt,data=tosum.c,FUN=c(mean))
(sums.m2[1,2]-sums.m2[2,2])/sums.m2[1,2] # 20% reduction in C uptake
(sums.m2[1,3]-sums.m2[2,3])/sums.m2[1,3] # 0.2% reduction in H2O loss

#- calculate % change in response to heatwave for photo and trans
#  for four heatwave days only
sums.m3 <- summaryBy(Photo.g+Trans.kg~HWtrt,data=tosum.HW,FUN=c(mean))
(sums.m3[1,2]-sums.m3[2,2])/sums.m3[1,2] # 40% reduction in C uptake
(sums.m3[1,3]-sums.m3[2,3])/sums.m3[1,3] # 5% reduction in H2O loss












#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot time courses of air temperature, photosynthesis and transpiraiton during the heatwave.
#- Zoom in on day 3 of the heatwave
wtc.m <- wtc.m[with(wtc.m, order(chamber, DateTime_hr)), ]
head(wtc.m) # flux data are hourly
head(dat) #note taht this requires that the script "plot_temperatures_heatwave.R" have already been run.

#- get hourly average leaf temperatures
dat.LC <- dat
dat.LC$DateTime_hr <- nearestTimeStep(dat.LC$DateTime_hr, nminutes = 60, align = "floor")
dat.LC2 <- summaryBy(Tleaf+TargTempC_Avg+PPFD_Avg~chamber+DateTime_hr,data=dat.LC,keep.names=T)

#- merge fluxes and temperatures
wtc.LC <- merge(wtc.m,dat.LC2,by=c("chamber","DateTime_hr"))
wtc.LC <- wtc.LC[with(wtc.LC, order(chamber, DateTime_hr)), ]
wtc.LC$WUEi <- with(wtc.LC,Photo/Trans)

#- average across treatments
day3.m <- summaryBy(.~DateTime_hr+HWtrt,data=subset(wtc.LC,as.Date(DateTime_hr-1) == as.Date("2016-11-2")),FUN=c(mean,se))


windows(80,120)
par(mar=c(2,6,1,6),oma=c(2,0,4,0),mfrow=c(5,1),cex.lab=1.6,las=1,cex.axis=1.2)
palette(c("blue","red"))

#---
#- plot PAR
plotBy(PAR.mean ~DateTime_hr|HWtrt,data=day3.m,type="l",lwd=3,lty=1,pch=16,
       ylab=expression(PPFD),ylim=c(0,2000),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
legend("topright",legend=letters[1],cex=2,bty="n")

#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|HWtrt,data=day3.m,type="l",lwd=3,lty=1,pch=16,
       ylab=expression(T[air]~and~T[leaf]),ylim=c(5,45),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=day3.m$DateTime_hr,y=day3.m$Tair_al.mean,SE=day3.m$Tair_al.se,
             direction="updown",col=day3.m$HWtrt,barlen=0,lwd=0.5)
#- overlay leaf temperature
plotBy(TargTempC_Avg.mean~DateTime_hr|HWtrt,data=day3.m,type="l",lwd=3,lty=3,pch=1,add=T,legend=F)
legend("topleft",c("Air","Leaf"),col="black",lty=c(1,3),lwd=c(2,2))
adderrorbars(x=day3.m$DateTime_hr,y=day3.m$TargTempC_Avg.mean,SE=day3.m$TargTempC_Avg.se,
             direction="updown",col=day3.m$HWtrt,barlen=0,lwd=0.5)

# add legend
legend(x=min(day3.m$DateTime_hr),y=130,xpd=NA,lwd=3,col=palette()[1:2],ncol=4,cex=1.5,bty="n",
       legend=c("Control","Heatwave"))
legend("topright",legend=letters[2],cex=2,bty="n")

#- plot VPD
plotBy(VPD.mean~DateTime_hr|HWtrt,data=day3.m,type="o",lwd=3,lty=1,pch=16,
       ylab=expression(VPD),ylim=c(0,6),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=day3.m$DateTime_hr,y=day3.m$VPD.mean,SE=day3.m$VPD.se,
             direction="updown",col=day3.m$HWtrt,barlen=0,lwd=0.5)
legend("topright",legend=letters[3],cex=2,bty="n")

#---
#- plot photosynthesis
plotBy(Photo.mean~DateTime_hr|HWtrt,data=day3.m,legend=F,type="o",lty=1,lwd=3,ylim=c(-1,10),las=1,pch=16,
       ylab=expression(A[canopy]));abline(h=0)
#- replot and add error bars
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=day3.m$DateTime_hr,y=day3.m$Photo.mean,SE=day3.m$Photo.se,
             direction="updown",col=day3.m$HWtrt,barlen=0,lwd=0.5)
legend("topright",legend=letters[4],cex=2,bty="n")


#---
#- plot transpiration
plotBy(Trans.mean~DateTime_hr|HWtrt,data=day3.m,legend=F,type="o",lty=1,lwd=3,ylim=c(0,2.5),las=1,pch=16,
       ylab=expression(E[canopy]));abline(h=0)
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=day3.m$DateTime_hr,y=day3.m$Trans.mean,SE=day3.m$Trans.se,
             direction="updown",col=day3.m$HWtrt,barlen=0,lwd=0.5)
legend("topright",legend=letters[5],cex=2,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Try a plot combining the timecourse of whole-tree fluxes and a bar chart of flux sums during the heatwave



windows(80,70)
par(mar=c(2,2,1,2),oma=c(4,5,2,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE), 
       widths=c(5,1), heights=c(1,3,3))
palette(c("blue","red"))


#---
#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|HWtrt,data=wtc.m2.two,type="l",lwd=3,lty=1,
       ylab=expression(T[air]~(degree*C)),ylim=c(5,45),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Tair_al.mean,SE=wtc.m2.two$Tair_al.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)

#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 18:00","2016-11-4 16:00"),format="%Y-%m-%d %R")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=55,col="darkgrey",density=7) 
#- add legend
legend(x=starttime+212000,y=68,xpd=NA,lwd=3,col=palette()[1:2],ncol=4,cex=1.5,bty="n",
       legend=c("Control","Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")

#empty plot
frame()

#---
#- plot photosynthesis timecourse
plotBy(Photo.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3,ylim=c(-1,12),las=1,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-5,xright=dates[2],ytop=20,col="darkgrey",density=7) #add rectangles for heatwave
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Photo.mean,SE=wtc.m2.two$Photo.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)
plotBy(Photo.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3.5,ylim=c(-1,12),las=1,add=T,
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))
legend("topright",legend=letters[2],cex=2,bty="n")


#-- plot photo bars
toplot.HW <- cbind(toplot2[,1],toplot3[,1],toplot2[,2],toplot3[,2])

xvals <- barplot(toplot2[,1], beside=T, ylab="", names.arg=rep("",4),yaxt="n",space=0,
                 cex.names=1.5, las=1, ylim=c(0,16), col=brewer.pal(6,"Set1")[3:6])
adderrorbars(x=xvals[,1],y=toplot.all3[,1],SE=sums.HW$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
legend(x=0.8,y=16,legend=letters[4],cex=2,bty="n")
title(ylab=expression(Total~A[canopy]~(g~m^-2)),outer=T,line=-60,cex.lab=1.8,xpd=NA,adj=0.7) # y-axis label
text(x=xvals[,1]+0.3,y=-0.8,labels=c("A:C","A:HW","W:C","W:HW"),xpd=T,srt=90,pos=2,cex=1.4,xpd=NA)

#---
#- plot transpiration timecourse
plotBy(Trans.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3,ylim=c(0,3),las=1,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-1,xright=dates[2],ytop=4,col="darkgrey",density=7) #add rectangles for heatwave
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Trans.mean,SE=wtc.m2.two$Trans.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)
plotBy(Trans.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3.5,ylim=c(0,3),las=1,add=T,
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
legend("topright",legend=letters[3],cex=2,bty="n")

#-- plot transpiration bars
xvals <- barplot(toplot2[,2], beside=T, ylab="", names.arg=rep("",4),yaxt="n",space=0,
                 cex.names=1.5, las=1, ylim=c(0,10), col=brewer.pal(6,"Set1")[3:6])
adderrorbars(x=xvals[,1],y=toplot2[,2],SE=sums.HW$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
legend(x=0.8,y=9,legend=letters[5],cex=2,bty="n")
title(ylab=expression(Total~E[canopy]~(kg~m^-2)),outer=T,line=-60,cex.lab=1.8,xpd=NA,adj=0.1) # y-axis label
text(x=xvals[,1]+0.3,y=-0.5,labels=c("A:C","A:HW","W:C","W:HW"),xpd=T,srt=90,pos=2,cex=1.4,xpd=NA)

#-- add left y-axis titles
title(ylab=expression(T[air]~(degree*C)),outer=T,adj=0.97,line=1)
title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,adj=0.7,line=1)
title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,adj=0.1,line=1)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------








###---- Plot bars for sums of heatwaveperiod separately

toplot.HW <- cbind(toplot2[,1],toplot3[,1],toplot2[,2],toplot3[,2])

windows()
par(oma=c(3,4,0,0))
palette(c("blue","darkgrey","orange","red"))

#- plot flux sums for CO2 (first 2 groups, HW, REC) and then H2O (last 2 groups, HW, REC)
xvals <- barplot(toplot.all3, beside=T, ylab="", names.arg=rep("",ncol(toplot.all3)),
                 cex.names=1.5, las=1, ylim=c(0,16), col=c("blue","darkgrey","orange","red"))
adderrorbars(x=xvals[,1],y=toplot.all3[,1],SE=sums.HW$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,2],y=toplot.all3[,2],SE=sums.REC$Photo.g.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,3],y=toplot.all3[,3],SE=sums.HW$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[,4],y=toplot.all3[,4],SE=sums.REC$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
abline(v=10.5)


box(bty="l")
graphics::text(x=c(5.5,16),y=16,xpd=NA,labels=c("Photosynthesis","Transpiration"),cex=1.8) # x-axis labels
title(ylab=expression(Total~Flux~(g~C~m^-2*";"~kg~H[2]*O~m^-2)),cex.lab=1.8,xpd=NA) # y-axis label
legend(x=11,y=13,legend=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"),
       fill=palette()[1:4],ncol=1,bty="b",cex=1.2)
text(x=xvals[3,]+.5,y=-1,labels=rep(c("Heatwave","Recovery"),2),xpd=T,srt=45,pos=2,cex=1.4)
###---- 


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
