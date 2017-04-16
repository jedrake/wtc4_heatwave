#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Plots the leaf water potential and hydraulic data during the heatwave
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------
#-- read in the midday leaf water potential data, average across treatments and dates
lwp1 <- read.csv("Data/WTC_TEMP_CM_PARRA_WATERPOTENTIAL-HEATWAVE_20161019-20161107_L0.csv")
lwp1$Date <- as.Date(lwp1$Date)

#- middays
lwp.midday <- summaryBy(LWP~chamber+T_treatment+HW_treatment+Date,
                 data=subset(lwp1,tissue=="leaf" & Timing=="midday"),FUN=mean,keep.names=T) # average over subreplicates

lwp.midday.m <- summaryBy(LWP~T_treatment+HW_treatment+Date,data=lwp.midday,FUN=c(mean,se)) # average over chambers (real reps)
lwp.midday.m.amb <- subset(lwp.midday.m,T_treatment=="ambient")
lwp.midday.m.ele <- subset(lwp.midday.m,T_treatment=="elevated")

#- predawns
lwp.predawn <- summaryBy(LWP~chamber+T_treatment+HW_treatment+Date,
                        data=subset(lwp1,tissue=="leaf" & Timing=="pre-dawn"),FUN=mean,keep.names=T) # average over subreplicates

lwp.predawn.m <- summaryBy(LWP~T_treatment+HW_treatment+Date,data=lwp.predawn,FUN=c(mean,se)) # average over chambers (real reps)
lwp.predawn.m.amb <- subset(lwp.predawn.m,T_treatment=="ambient")
lwp.predawn.m.ele <- subset(lwp.predawn.m,T_treatment=="elevated")

#----------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------
#-- read in the turgor loss point measurements, get average for each T_treatment
tlp1 <- read.csv("Data/WTC_TEMP_CM_PARRA_TURGOR-LOSS-POINT-HEATWAVE_20161104_L0.csv")
tlp <- summaryBy(TLP~chamber+T_treatment+HW_treatment,data=tlp1,FUN=mean,keep.names=T) # average over subreplicates

tlp.m <- summaryBy(TLP~T_treatment,data=tlp,FUN=c(mean,se)) # average over chambers (real reps)
#----------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------
#-- read in the Kleaf data, get average for each T_treatment and HW_treatment combination
Kleaf1 <- read.csv("Data/WTC_TEMP_CM_PARRA_KLEAF-HEATWAVE_20161104_L0.csv")
Kleaf <- summaryBy(Kleaf~chamber+T_treatment+HW_treatment,data=Kleaf1,FUN=mean,keep.names=T) # average over subreplicates

Kleaf.m <- summaryBy(Kleaf~T_treatment+HW_treatment,data=Kleaf,FUN=c(mean,se)) # average over chambers (real reps)
#----------------------------------------------------------------------------------------------------------








#----------------------------------------------------------------------------------------------------------
#- plot

#- set up plot
#windows(35,15)
pdf("Output/FigureS6-water_potential.pdf",width=12,height=5)
par(mfrow=c(1,2),mar=c(6,7,2,1.5),cex.axis=1.5)
symbols <- c(1,16)
ptsize <- 2
palette(c("blue","black","orange","red"))

#----
#-- plot the ambient treatment
plot(LWP.mean~Date,data=subset(lwp.midday.m.amb,HW_treatment=="control"),ylim=c(-3,0),type="o",las=1,
     pch=21,bg="white",ylab="",xlab="")
#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-5"),format="%Y-%m-%d")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=1,col="darkgrey",density=10) #add rectangles for droughts

#- plot the midday leaf water potentials
adderrorbars(x=lwp.midday.m.amb$Date,y=lwp.midday.m.amb$LWP.mean,SE=lwp.midday.m.amb$LWP.se,direction="updown",barlen=0)
points(LWP.mean~Date,data=subset(lwp.midday.m.amb,HW_treatment=="control"),ylim=c(-3,0),type="o",
       pch=21,bg="blue",cex=ptsize)
points(LWP.mean~Date,data=subset(lwp.midday.m.amb,HW_treatment=="heatwave"),ylim=c(-3,0),type="o",
       pch=21,bg="black",cex=ptsize)

#- plot the predawn leaf water potentials
adderrorbars(x=lwp.predawn.m.amb$Date,y=lwp.predawn.m.amb$LWP.mean,SE=lwp.predawn.m.amb$LWP.se,direction="updown",barlen=0)
points(LWP.mean~Date,data=subset(lwp.predawn.m.amb,HW_treatment=="control"),ylim=c(-3,0),type="o",
       pch=21,bg="white",col="blue",cex=ptsize)
points(LWP.mean~Date,data=subset(lwp.predawn.m.amb,HW_treatment=="heatwave"),ylim=c(-3,0),type="o",
       pch=21,bg="white",col="black",cex=ptsize)

#- add a line for the turgor loss point
abline(h=tlp.m$TLP.mean[1],lwd=2,lty=2)
textxy(X=as.Date("2016-10-27"),Y=tlp.m$TLP.mean[1],lab="Turgor loss point",offset=-1,cex=1)

#- label axes and add a legend
title(main=expression(Ambient~temperature),cex.main=1.5)
title(ylab=expression(Psi[L]~(MPa)),cex.lab=2)
legend("bottomleft",legend=c("Predawn-Control","Predawn-Heatwave","Midday-Control","Midday-Heatwave"),cex=0.9,
       pch=21,col=c("blue","black"),pt.bg=c("white","white","blue","black"),pt.cex=ptsize,ncol=2,
       bg="white")
box()
legend("bottomright",legend=letters[1],cex=2,bty="n")

#----
#-- plot the warmed treatment
plot(LWP.mean~Date,data=subset(lwp.midday.m.ele,HW_treatment=="control"),ylim=c(-3,0),type="o",las=1,
     pch=21,bg="white",ylab="",xlab="")

#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-5"),format="%Y-%m-%d")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=1,col="darkgrey",density=10) #add rectangles for droughts

#- plot the midday leaf water potentials
adderrorbars(x=lwp.midday.m.ele$Date,y=lwp.midday.m.ele$LWP.mean,SE=lwp.midday.m.ele$LWP.se,direction="updown",barlen=0)
points(LWP.mean~Date,data=subset(lwp.midday.m.ele,HW_treatment=="control"),ylim=c(-3,0),type="o",
       pch=21,bg="orange",cex=ptsize)
points(LWP.mean~Date,data=subset(lwp.midday.m.ele,HW_treatment=="heatwave"),ylim=c(-3,0),type="o",
       pch=21,bg="red",cex=ptsize)

#- plot the predawn leaf water potentials
adderrorbars(x=lwp.predawn.m.ele$Date,y=lwp.predawn.m.ele$LWP.mean,SE=lwp.predawn.m.ele$LWP.se,direction="updown",barlen=0)
points(LWP.mean~Date,data=subset(lwp.predawn.m.ele,HW_treatment=="control"),ylim=c(-3,0),type="o",
       pch=21,bg="white",col="orange",cex=ptsize)
points(LWP.mean~Date,data=subset(lwp.predawn.m.ele,HW_treatment=="heatwave"),ylim=c(-3,0),type="o",
       pch=21,bg="white",col="red",cex=ptsize)

#- add a line for the turgor loss point
abline(h=tlp.m$TLP.mean[2],lwd=2,lty=2)
textxy(X=as.Date("2016-10-27"),Y=tlp.m$TLP.mean[2],lab="Turgor loss point",offset=-1,cex=1)

#- label axes and add a legend
title(main=expression(Warmed~temperature),cex.main=1.5)
title(ylab=expression(Psi[L]~(MPa)),cex.lab=2)
legend("bottomleft",legend=c("Predawn-Control","Predawn-Heatwave","Midday-Control","Midday-Heatwave"),cex=0.9,
       pch=21,col=c("orange","red"),pt.bg=c("white","white","orange","red"),pt.cex=ptsize,ncol=2,
       bg="white")
box()
legend("bottomright",legend=letters[2],cex=2,bty="n")

dev.off()
#----------------------------------------------------------------------------------------------------------








#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Plot branch conductivity and leaf hydraulic conductance
embolism <- read.csv("Data/WTC_TEMP_CM_PARRA_NATIVE-EMBOLISM-HEATWAVE_20161018-20161104_L0_v2.csv")
embolism$combotrt <- factor(paste(embolism$T_treatment,embolism$HW_treatment,sep="_"))
embolism$Date <- as.Date(embolism$Date)
embolism$phase <- factor(ifelse(embolism$Date == as.Date("2016-10-18"),"pre","post"))

#- average across branches, then phases
embolism2 <- summaryBy(plc+Ksi+KSf~chamber+T_treatment+HW_treatment+phase+combotrt,data=embolism,FUN=mean,keep.names=T)
embolism.m <- summaryBy(plc+Ksi+KSf~T_treatment+HW_treatment+phase+combotrt,data=embolism2,FUN=c(mean,se))


#------
#- pull out the means to plot native and rehydrated branch conductivity
A_c <- embolism.m[which(embolism.m$combotrt=="ambient_control"),]
A_HW <- embolism.m[which(embolism.m$combotrt=="ambient_heatwave"),]
E_c <- embolism.m[which(embolism.m$combotrt=="elevated_control"),]
E_HW <- embolism.m[which(embolism.m$combotrt=="elevated_heatwave"),]

toplot   <- as.matrix(rbind(A_c[,6:7], A_HW[,6:7], E_c[,6:7], E_HW[,6:7]))
ses <-  as.matrix(rbind(A_c[,9:10], A_HW[,9:10], E_c[,9:10], E_HW[,9:10]))
rownames(toplot) <- c("A_c", "A_HW", "E_c","E_HW")
colnames(toplot) <- c("Pre","Post")


#windows(80,50)
pdf("Output/FigureS7-hydraulics.pdf",width=12,height=5)
par(mar=c(6,8,1,1),oma=c(0,0,0,0),mfrow=c(1,2))
xvals <- barplot(t(toplot), beside=T, ylab="", names.arg=rep("",8),
                 cex.names=1.5, las=1, ylim=c(0,1.5), col=c("blue","blue","darkgrey","darkgrey","orange","orange","red","red"))

#- add shading
xvals <- barplot(t(toplot), beside=T, ylab="", names.arg=rep("",8),add=T,
                 cex.names=1.5, las=1, ylim=c(0,20), col="black",
                 density=rep(c(0,7),4))
adderrorbars(x=xvals[1,],y=embolism.m$Ksi.mean,SE=embolism.m$Ksi.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
adderrorbars(x=xvals[2,],y=embolism.m$KSf.mean,SE=embolism.m$KSf.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
text(x=xvals[2,]+1,y=-0.1,cex=1.5,
     labels=c("AC","AH","WC","WH"),xpd=T,srt=0,pos=2)
title(ylab=expression(atop(Branch~conductivity,
                           (Kg~m^-1~s^-1~MPa^-1))),cex.lab=1.8,xpd=NA) # y-axis label
legend("topleft",col="black",legend=c("Native","Rehydrated"),
       ncol=1, fill="black", bty="n",
       cex=1.3, seg.len = 6, angle=45, density=c(0,15))

box()
text(x=c(7,18),y=-0.2,labels=c("Pre-heatwave","Heatwave"),xpd=T,cex=1.5)
abline(v=12.5,lty=2)
legend("topright",letters[1],cex=2,bty="n")
#------



#------
#- add another plot for leaf hydraulic conductivity
xvals <- barplot(Kleaf.m$Kleaf.mean, beside=T, ylab="", names.arg=c("AC","AH","WC","WH"),space=0.5,
                 cex.names=1.5, las=1, ylim=c(0,30), col=c("blue","darkgrey","orange","red"))
adderrorbars(x=xvals,y=Kleaf.m$Kleaf.mean,SE=Kleaf.m$Kleaf.se,
             direction="updown",col="black",barlen=0.05,lwd=0.5)
box()
legend("topright",letters[2],cex=2,bty="n")
title(ylab=expression(atop(Leaf~hydraulic~conductance,
                           (mmol~m^-2~s^-1~MPa^-1))),cex.lab=1.8,xpd=NA) # y-axis label
dev.off()
#------
