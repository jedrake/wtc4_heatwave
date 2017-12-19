#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Script to plot the whole-tree flux data during the heatwave
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- Read in and process the flux data associated with the heatwave.
wtc <- read.csv("Data/WTC_TEMP-PARRA_CM_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv")

wtc$DateTime <- as.POSIXct(wtc$DateTime_hr,format="%Y-%m-%d %T",tz="GMT")

#-- create hourly values for subsequent averaging of fluxes
wtc$DateTime_hr <- nearestTimeStep(wtc$DateTime,nminutes=60,align="floor")
wtc.m1 <- summaryBy(PAR+VPD+Tair_al+Photo+Trans+TargTempC_Avg~DateTime_hr+chamber+T_treatment,data=wtc,#subset(wtc,DoorCnt==0),
                   FUN=mean,keep.names=T,na.rm=T)

#- merge in the heatwave treatment key
linkdf <- data.frame(chamber = levels(as.factor(wtc$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08

wtc.m <- merge(wtc.m1,linkdf,by="chamber")
wtc.m$combotrt <- factor(paste(wtc.m$T_treatment,wtc.m$HWtrt,sep="_"))

#- average and SEs for each treatment. 
wtc.m2 <- summaryBy(.~DateTime_hr+combotrt,data=subset(wtc.m1,as.Date(DateTime_hr) < as.Date("2016-11-11")),FUN=c(mean,se))
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

#- calculate mean and SE of chamber sums, for grouped bar chart
sums.m <- summaryBy(Photo.g+Trans.kg~HWtrt,data=tosum.c,FUN=c(mean,se))

#- pull out the means to plot
Cdat <- sums.m[1,]
HW_dat <- sums.m[2,]
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot time courses of air temperature, photosynthesis and transpiraiton during the heatwave.
starttime <- as.POSIXct("2016-10-29 00:00:00",tz="GMT")
wtc.m2.two <- summaryBy(.~DateTime_hr+HWtrt,
                       data=subset(wtc.m,as.Date(DateTime_hr) <= as.Date("2016-11-4") & as.Date(DateTime_hr) >= as.Date("2016-10-30")),FUN=c(mean,se))




pdf("Output/Figure2_fluxes.pdf")
#windows(80,70)
par(mar=c(2,2,1,2),oma=c(4,5,2,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE), 
       widths=c(5,1.5), heights=c(2,3,3))

palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red

#---
#- plot Tair
plotBy(Tair_al.mean~DateTime_hr|HWtrt,data=wtc.m2.two,type="l",lwd=3,lty=1,col=c("blue","red"),
       ylab=expression(T[air]~(degree*C)),ylim=c(5,45),legend=F)

#- add VPD to the right y-axis
par(new=T)
plotBy(VPD.mean~DateTime_hr|HWtrt,data=wtc.m2.two,type="l",lwd=2,lty=2,col=c("blue","red"),
       axes=F,xlab=NA,ylab=NA,legend=F,ylim=c(0,6))
axis(side=4)
graphics::text(x=max(wtc.m2.two$DateTime_hr)+8e4, y = 3, expression(VPD~(kPa)),srt=90,xpd=NA,cex=1.6)
legend("topleft",legend=c(expression(T[air]),"VPD"),lty=c(1,2),lwd=c(2,2),col="red",seg.len=3,bty="n")


#- add shaded rectangle for heatwave
dates <- as.POSIXct(c("2016-10-31 02:00","2016-11-3 24:00"),format="%Y-%m-%d %R")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=55,col="darkgrey",density=7) 
#- add legend
legend(x=starttime+212000,y=59,xpd=NA,lwd=3,col=c("blue","red"),ncol=4,cex=1.5,bty="n",
       legend=c("Control","Heatwave"))
legend("topright",legend=paste("(",letters[1],")",sep=""),cex=1.5,bty="n",text.font=2)
plotBy(Tair_al.mean~DateTime_hr|HWtrt,data=wtc.m2.two,type="l",lwd=3,lty=1,col=c("blue","red"),
       ylab=expression(T[air]~(degree*C)),ylim=c(5,45),legend=F,add=T)

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
legend("topright",legend=paste("(",letters[2],")",sep=""),cex=1.5,bty="n",text.font=2)


#-- plot photo bars
toplot.HW <- cbind(Cdat[,2],HW_dat[,2])
 
xvals <- barplot(toplot.HW[1:2], beside=T, ylab="", names.arg=c("C","H"),yaxt="n",space=0,xaxt="n",
                  cex.names=1.3, las=1, ylim=c(0,16), col=c("blue","red"))#col=brewer.pal(6,"Set1")[3:6])
axis(1,at=xvals,labels=c("C","HW"),cex.axis=1.5,tck=0)
adderrorbars(x=xvals[,1],y=toplot.HW[1:2],SE=sums.m$Photo.g.se,
              direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
title(ylab=expression(Total~A[canopy]~(g~m^-2)),outer=T,line=-47,cex.lab=1.8,xpd=NA,adj=0.6) # y-axis label
#text(x=xvals[,1]+0.6,y=-1.2,labels=c("C ","HW"),xpd=T,srt=00,pos=2,cex=1.7,xpd=NA)
legend("topright",legend=paste("(",letters[4],")",sep=""),cex=1.5,bty="n",text.font=2)


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
legend("topright",legend=paste("(",letters[3],")",sep=""),cex=1.5,bty="n",text.font=2)

#-- plot transpiration bars
toplot.HW <- cbind(Cdat[,3],HW_dat[,3])

xvals <- barplot(toplot.HW[1:2], beside=T, ylab="", names.arg=c("C","HW"),yaxt="n",space=0,xaxt="n",
                 cex.names=1, las=1, ylim=c(0,7), col=c("blue","red"))#col=brewer.pal(6,"Set1")[3:6])
axis(1,at=xvals,labels=c("C","HW"),cex.axis=1.5,tck=0)

adderrorbars(x=xvals[,1],y=toplot.HW[1:2],SE=sums.m$Trans.kg.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
legend("topright",legend=paste("(",letters[5],")",sep=""),cex=1.5,bty="n",text.font=2)
title(ylab=expression(Total~E[canopy]~(kg~m^-2)),outer=T,line=-47,cex.lab=1.8,xpd=NA,adj=0.1) # y-axis label


#-- add left y-axis titles
title(ylab=expression(T[air]~(degree*C)),outer=T,adj=0.92,line=1)
title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,adj=0.6,line=1)
title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,adj=0.01,line=1)

dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------











#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the average  Acanopy and Ecanopy during the hot afternoon during the heatwave for each treatment.
hw1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3"))
hw <- subset(hw1,hour(DateTime_hr) >= 12 & hour(DateTime_hr) <= 14)
hw$RH <- VPDtoRH(VPD=hw$VPD,TdegC=hw$Tair_al)

#- average across chambers, do simple stats test. Significant HW effects for Photo & Trans
hw.chamber <- summaryBy(Photo+Trans~chamber+T_treatment+HWtrt,data=hw,FUN=mean,keep.names=T)
lm.photo.hw <- lm(Photo~T_treatment*HWtrt,data=hw.chamber)
Anova(lm.photo.hw)
lm.Trans.hw <- lm(Trans~T_treatment*HWtrt,data=hw.chamber)
Anova(lm.Trans.hw)


#- do the same, but for the three days after the heatwave
post1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-11-4") & as.Date(DateTime_hr) <= as.Date("2016-11-6"))
post <- subset(post1,hour(DateTime_hr) >= 10 & hour(DateTime_hr) <= 16)

#- average across chambers, do simple stats test. Significant HW effects for Photo & Trans
post.chamber <- summaryBy(Photo+Trans~chamber+T_treatment+HWtrt,data=post,FUN=mean,keep.names=T)
lm.photo.post <- lm(Photo~T_treatment*HWtrt,data=post.chamber)
Anova(lm.photo.post)
lm.Trans.post <- lm(Trans~T_treatment*HWtrt,data=post.chamber)
Anova(lm.Trans.post)


#- calculate the average Ecanopy during the night during the heatwave for each treatment.
hw.night1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3") & PAR < 4)
hw.night <- subset(hw.night1,hour(DateTime_hr) <= 4 | hour(DateTime_hr) >= 22)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------











#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate % change in response to heatwave for photo and trans
#  for four heatwave days only
sums.m2 <- summaryBy(Photo.g+Trans.kg~HWtrt,data=tosum.c,FUN=c(mean))
(sums.m2[1,2]-sums.m2[2,2])/sums.m2[1,2] # 40% reduction in C uptake
(sums.m2[1,3]-sums.m2[2,3])/sums.m2[1,3] # 5% reduction in H2O loss


#-------
#- statistical analysis of integrated heatwave effects on C uptake and H2O loss.

#- sum across chambers. Note that there are a few NA's, which will modestly affect the sums.
HW_sums_chamber <- summaryBy(Photo.g+Trans.kg~chamber+T_treatment+HWtrt+combotrt,
                     data=subset(tosum,as.Date(DateTime_hr)<=as.Date("2016-11-04")),FUN=sum,keep.names=T,na.rm=T)


lm.C.sums <- lm(Photo.g~T_treatment*HWtrt,data=HW_sums_chamber)
Anova(lm.C.sums)

lm.W.sums <- lm(Trans.kg~T_treatment*HWtrt,data=HW_sums_chamber)
Anova(lm.W.sums)
#-------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- make a plot of WUEi vs. T, as requested by Adrienne Nicotra
hw1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3"))
hw1$WUE <- with(hw1,Photo/Trans)



#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#- Make 2-panel density plot, showing WUE for both treatments

#windows(120,70)
pdf("Output/FigureS5-WUE.pdf",width=12,height=6)
par(mar=c(6,6,1,1),mfrow=c(1,2),oma=c(0,0,0,0),cex.lab=1.5,las=1,cex.axis=1.2)
# layout(matrix(c(1,2), 1, 2, byrow = TRUE), 
#        widths=c(2,2), heights=c(2,2))
ptsize=1.2
PARlimit=600

hw.cont <- subset(hw1,PAR>PARlimit & HWtrt=="C")
hw.hw <- subset(hw1,PAR>PARlimit & HWtrt=="HW")

#- set the color palette
blues.ramp <- colorRampPalette(brewer.pal(9,"Blues")[-1],alpha=0.7)
reds.ramp <- colorRampPalette(brewer.pal(9,"Reds")[-1],alpha=0.7)


#- plot Photosynthesis vs. Tleaf. 
dc <- densCols(hw.cont$WUE,hw.cont$TargTempC_Avg,colramp=blues.ramp)
dh <- densCols(hw.hw$WUE,hw.hw$TargTempC_Avg,colramp=reds.ramp)

plot(hw.cont$WUE~hw.cont$TargTempC_Avg,col=dc,pch=16,cex=ptsize,ylim=c(-1,30),xlim=c(15,45),xaxt="n",yaxt="n",ylab="",xlab="")
points(hw.hw$WUE~hw.hw$TargTempC_Avg,col=dh,pch=16,cex=ptsize,ylim=c(-5,60),xlim=c(15,45),xaxt="n",yaxt="n")
abline(h=0)
axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
legend("topright",c("Control","Heatwave"),pch=c(16,16),col=c(dc[15],dh[3]),cex=1.5,bg="white",title="Treatment")
legend("topleft",letters[1],bty="n",cex=1.5)

title(xlab=expression(Canopy~temperature~(degree*C*";"~T[L-IR])),outer=F,cex.lab=2,line=4)
title(ylab=expression(WUE[i]~(mu*mol~CO[2]~"/"~mmol~H[2]*O)),outer=F,cex.lab=2)


#--- add Photo vs. Transpiratio, as requested by Peter
#- Make plot requested by Peter


#- Read in the data, do some formatting.
dat.all <- read.csv("Data/WTC_TEMP-PARRA_CM_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv")
dat.all$DateTime_hr <- as.POSIXct(dat.all$DateTime_hr,format="%Y-%m-%d %T",tz="GMT")
dat.all$Tdiff <- with(dat.all,TargTempC_Avg-Tair_al)

# Calculate conductance as the simple ratio between Trans and VPD. But use leaf to air VPD. Leaf to air VPD tends to be greater than air VPD, as leaves tend to be warmer than air. Plot the relationships between photosynthesis, conductance, and leaftoairVPD. Note the shift in points at extreme temperatures and VPD in teh heatwave treatment, consistent with lower photosynthesis than would be expected given the measured gs.
dat.all$leaftoairVPD <- VPDairToLeaf(VPD=dat.all$VPD,Tair=dat.all$Tair_al,Tleaf=dat.all$TargTempC_Avg)
dat.all$gs <- dat.all$Trans/dat.all$leaftoairVPD/10
dat.all$Atogs <- with(dat.all,Photo/(gs*1000))

cdat.all <- subset(dat.all,HWtrt=="C" & as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-4"))
hwdat.all <- subset(dat.all,HWtrt=="HW" & as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-4"))

#- plot
dat <- rbind(subset(cdat.all,PAR>600),subset(hwdat.all,PAR>600))
dat$T_cat <- .bincode(dat$TargTempC_Avg,breaks=c(10,20,25,30,35,40,50))

colors <- rev(brewer.pal(6,"RdYlGn"))
plot(Photo~Trans,data=dat,col=colors[T_cat],cex=0.75,pch=16,xlim=c(0,4),ylim=c(0,12),xlab="",ylab="")
title(xlab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)))

legend("topright",legend=c("15-20","20-25","25-30","30-35","35-40",">40"),col=colors,pch=16,title=expression(T[L-IR]))
legend("topleft",letters[2],bty="n",cex=1.5)
dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
















#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the daytime and nighttime sums for C gain, as requested by reviewer #3
hw1 <- subset(wtc.m,as.Date(DateTime_hr) >= as.Date("2016-10-31") & as.Date(DateTime_hr) <= as.Date("2016-11-3"))
hw1$Photo.g <- hw1$Photo*1e-6*12.011*60*60
hw.day <- subset(hw1,hour(DateTime_hr) >= 6 & hour(DateTime_hr) <= 20 & PAR >10)
hw.night1 <- subset(hw1,hour(DateTime_hr) <= 6 | hour(DateTime_hr) >= 20)
hw.night <- subset(hw.night1,PAR <1)


###--- The entire heatwvave 4-day period
#- sum across chambers during the DAY. Note that there are a few NA's, which will modestly affect the sums.
tosum.day.c <- summaryBy(Photo.g~chamber+HWtrt,data=hw.day,FUN=sum,keep.names=T,na.rm=T)
tosum.night.c <- summaryBy(Photo.g~chamber+HWtrt,data=hw.night,FUN=sum,keep.names=T,na.rm=T)


#- calculate mean and SE of chamber sums, for grouped bar chart
sums.day.m <- summaryBy(Photo.g~HWtrt,data=tosum.day.c,FUN=c(mean,se))
sums.night.m <- summaryBy(Photo.g~HWtrt,data=tosum.night.c,FUN=c(mean,se))

#- pull out the means to plot
Ddat <- sums.day.m
Ndat <- sums.night.m

toplot.HW <- data.frame(rbind(Ddat,c("C",0,0),c("C",0,0),Ndat))

#- plot grouped bar chart
#windows(20,20)
pdf("Output/FigureS4_fluxsums_day_night.pdf")
par(mar=c(7,7,1,1))
xvals <- barplot(as.numeric(toplot.HW[1:6,2]), beside=T, ylab="", names.arg=NA,#c("C","HW"," "," ","C","HW"),
                 #yaxt="n",space=0,xaxt="n",
                 cex.names=1, las=1, ylim=c(-3,15), col=c("blue","red"))#col=brewer.pal(6,"Set1")[3:6])
axis(1,at=xvals,c("C","HW"," "," ","C","HW"),cex.axis=1.5,tck=0)
adderrorbars(x=xvals[,1],y=as.numeric(toplot.HW[1:6,2]),SE=as.numeric(toplot.HW[1:6,3]),
           direction="updown",col="black",barlen=0.05,lwd=0.5)
axis(4)
box()
abline(h=0)
title(ylab=expression(Total~A[canopy]~(g~m^-2)),outer=T,line=-5,cex.lab=1.8,xpd=NA,adj=0.6) # y-axis label
text(x=c(1.2,6.2),y=-2,c("Day-time","Night-time"),cex=1.5)
dev.off()
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



