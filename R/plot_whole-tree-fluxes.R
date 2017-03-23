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

wtc1 <- read.csv("Data/WTC_TEMP-PARRA_CM_WTCFLUX_20161028-20161115_L0.csv")

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
#- sum across heatwave period, but for two treatments only!
#- subset to the desired timeframe, convert to g C and kg H2O hr-1
tosum <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-4"))
tosum$Photo.g <- tosum$Photo*1e-6*12.011*60*60
tosum$Trans.kg <- tosum$Trans*1e-3*18.015*60*60/1000



###--- The entire heatwvave 4-day period
#- sum across chambers. Note that there are a few NA's, which will modestly affect the sums.
tosum.c <- summaryBy(Photo.g+Trans.kg~chamber+HWtrt,data=tosum,FUN=sum,keep.names=T,na.rm=T)
boxplot(Photo.g~HWtrt,data=tosum.c) # reduction with heatwave
boxplot(Trans.kg~HWtrt,data=tosum.c) # no change

#- calculate mean and SE of chamber sums, make grouped bar chart
sums.m <- summaryBy(Photo.g+Trans.kg~HWtrt,data=tosum.c,FUN=c(mean,se))


#- pull out the means to plot
Cdat <- sums.m[1,]
HW_dat <- sums.m[2,]





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot time courses of air temperature, photosynthesis and transpiraiton during the heatwave.
#  TWO TREATMENTS
# 
wtc.m2.two <- summaryBy(.~DateTime_hr+HWtrt,
                       #data=subset(wtc.m,as.Date(DateTime_hr) <= as.Date("2016-11-6") & as.Date(DateTime_hr) >= as.Date("2016-10-30")),FUN=c(mean,se))
                       data=subset(wtc.m,as.Date(DateTime_hr) <= as.Date("2016-11-4") & as.Date(DateTime_hr) >= as.Date("2016-10-30")),FUN=c(mean,se))


windows(80,70)
par(mar=c(2,2,1,2),oma=c(4,5,2,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE), 
       widths=c(5,1), heights=c(2,3,3))

palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red

#---
#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|HWtrt,data=wtc.m2.two,type="l",lwd=3,lty=1,col=c("blue","red"),
       ylab=expression(T[air]~(degree*C)),ylim=c(5,45),legend=F)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Tair_al.mean,SE=wtc.m2.two$Tair_al.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)

#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 18:00","2016-11-4 16:00"),format="%Y-%m-%d %R")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=55,col="darkgrey",density=7) 
#- add legend
legend(x=starttime+212000,y=59,xpd=NA,lwd=3,col=c("blue","red"),ncol=4,cex=1.5,bty="n",
       legend=c("Control","Heatwave"))
legend("topright",legend=letters[1],cex=2,bty="n")

#empty plot
frame()

#---
#- plot photosynthesis timecourse
plotBy(Photo.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3,ylim=c(-1,12),las=1,col=c("blue","red"),
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-5,xright=dates[2],ytop=20,col="darkgrey",density=7) #add rectangles for heatwave
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Photo.mean,SE=wtc.m2.two$Photo.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)
plotBy(Photo.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3.5,ylim=c(-1,12),las=1,add=T,col=c("blue","red"),
       ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))
legend("topright",legend=letters[2],cex=2,bty="n")


#- two treatmetns only
toplot.HW <- cbind(Cdat[,2],HW_dat[,2])
 
xvals <- barplot(toplot.HW[1:2], beside=T, ylab="", names.arg=rep("",2),yaxt="n",space=0,
                  cex.names=1.5, las=1, ylim=c(0,16), col=c("blue","red"))#col=brewer.pal(6,"Set1")[3:6])
adderrorbars(x=xvals[,1],y=toplot.HW[1:2],SE=sums.m$Photo.g.se,
              direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
#legend(x=0.4,y=16,legend=letters[4],cex=2,bty="n")
title(ylab=expression(Total~A[canopy]~(g~m^-2)),outer=T,line=-60,cex.lab=1.8,xpd=NA,adj=0.6) # y-axis label
text(x=xvals[,1]+0.6,y=-1.2,labels=c("C ","HW"),xpd=T,srt=00,pos=2,cex=1.7,xpd=NA)
legend(x=0.4,y=16,legend=letters[4],cex=2,bty="n")


#---
#- plot transpiration timecourse
plotBy(Trans.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3,ylim=c(0,3),las=1,col=c("blue","red"),
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
rect(xleft=dates[1],ybottom=-1,xright=dates[2],ytop=4,col="darkgrey",density=7) #add rectangles for heatwave
axis(side = 4,ylab="",col="black",col.axis="black",las=1)
adderrorbars(x=wtc.m2.two$DateTime_hr,y=wtc.m2.two$Trans.mean,SE=wtc.m2.two$Trans.se,
             direction="updown",col=wtc.m2.two$HWtrt,barlen=0,lwd=0.5)
plotBy(Trans.mean~DateTime_hr|HWtrt,data=wtc.m2.two,legend=F,type="l",lty=1,lwd=3.5,ylim=c(0,3),las=1,add=T,col=c("blue","red"),
       ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)));abline(h=0)
legend("topright",legend=letters[3],cex=2,bty="n")



#- two treatmetns only
toplot.HW <- cbind(Cdat[,3],HW_dat[,3])

xvals <- barplot(toplot.HW[1:2], beside=T, ylab="", names.arg=rep("",2),yaxt="n",space=0,
                 cex.names=1.5, las=1, ylim=c(0,7), col=c("blue","red"))#col=brewer.pal(6,"Set1")[3:6])
adderrorbars(x=xvals[,1],y=toplot.HW[1:2],SE=sums.m$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
legend(x=0.4,y=7.5,legend=letters[5],cex=2,bty="n")
title(ylab=expression(Total~E[canopy]~(kg~m^-2)),outer=T,line=-60,cex.lab=1.8,xpd=NA,adj=0.1) # y-axis label
text(x=xvals[,1]+0.6,y=-0.8,labels=c("C  ","HW"),xpd=T,srt=00,pos=2,cex=1.7,xpd=NA)


#-- add left y-axis titles
title(ylab=expression(T[air]~(degree*C)),outer=T,adj=0.92,line=1)
title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,adj=0.65,line=1)
title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,adj=0.1,line=1)
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
#- average across chambers, do simple stats test. Significant HW effects for Photo & Trans
hw.chamber <- summaryBy(Photo+Trans~chamber+T_treatment+HWtrt,data=hw,FUN=mean,keep.names=T)
lm.photo.hw <- lm(Photo~T_treatment*HWtrt,data=hw.chamber)
Anova(lm.photo.hw)
lm.Trans.hw <- lm(Trans~T_treatment*HWtrt,data=hw.chamber)
Anova(lm.Trans.hw)


#- do the same, but for the three days after the heatwave
post1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-11-4") & as.Date(DateTime_hr) <= as.Date("2016-11-6"))
post <- subset(post1,hour(DateTime_hr) >= 10 & hour(DateTime_hr) <= 16)

boxplot(Photo~combotrt,data=post)
summaryBy(Photo~HWtrt,data=post)
summaryBy(Trans~HWtrt,data=post)
#- average across chambers, do simple stats test. Significant HW effects for Photo & Trans
post.chamber <- summaryBy(Photo+Trans~chamber+T_treatment+HWtrt,data=post,FUN=mean,keep.names=T)
lm.photo.post <- lm(Photo~T_treatment*HWtrt,data=post.chamber)
Anova(lm.photo.post)
lm.Trans.post <- lm(Trans~T_treatment*HWtrt,data=post.chamber)
Anova(lm.Trans.post)


#- calculate the average Ecanopy during the night during the heatwave for each treatment.
hw.night1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3") & PAR < 4)
hw.night <- subset(hw.night1,hour(DateTime_hr) <= 4 | hour(DateTime_hr) >= 22)

boxplot(Trans~combotrt,data=hw.night)
summaryBy(Photo~HWtrt,data=hw.night)
summaryBy(Trans~HWtrt,data=hw.night)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------