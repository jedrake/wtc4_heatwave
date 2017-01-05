#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Plots the leaf water potential and hydraulic data during the heatwave
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

source("R/loadLibraries.R")




#----------------------------------------------------------------------------------------------------------
#-- read in the midday leaf water potential data, average across treatments and dates
lwp1 <- read.csv("Data/Hydraulics/WTC_TEMP_CM_PARRA_WATERPOTENTIAL-MIDDAY-HEATWAVE_20161019-20161107_L0.csv")
lwp1$Date <- as.Date(lwp1$Date)
lwp <- summaryBy(LWP~chamber+T_treatment+HW_treatment+Date,data=lwp1,FUN=mean,keep.names=T) # average over subreplicates

lwp.m <- summaryBy(LWP~T_treatment+HW_treatment+Date,data=lwp,FUN=c(mean,se)) # average over chambers (real reps)
lwp.m.amb <- subset(lwp.m,T_treatment=="ambient")
lwp.m.ele <- subset(lwp.m,T_treatment=="elevated")
#----------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------
#-- read in the turgor loss point measurements, get average for each T_treatment
tlp1 <- read.csv("Data/Hydraulics/WTC_TEMP_CM_PARRA_TURGOR-LOSS-POINT-HEATWAVE_20161104_L0.csv")
tlp <- summaryBy(TLP~chamber+T_treatment+HW_treatment,data=tlp1,FUN=mean,keep.names=T) # average over subreplicates

tlp.m <- summaryBy(TLP~T_treatment,data=tlp,FUN=c(mean,se)) # average over chambers (real reps)
#----------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------
#-- read in the Kleaf data, get average for each T_treatment and HW_treatment combination
Kleaf1 <- read.csv("Data/Hydraulics/WTC_TEMP_CM_PARRA_KLEAF-HEATWAVE_20161104_L0.csv")
Kleaf <- summaryBy(Kleaf~chamber+T_treatment+HW_treatment,data=Kleaf1,FUN=mean,keep.names=T) # average over subreplicates

Kleaf.m <- summaryBy(Kleaf~T_treatment+HW_treatment,data=Kleaf,FUN=c(mean,se)) # average over chambers (real reps)
#----------------------------------------------------------------------------------------------------------








#----------------------------------------------------------------------------------------------------------
#- plot

#- set up plot
windows(35,20)
par(mfrow=c(1,2),mar=c(6,7,2,1),cex.axis=1.5)
symbols <- c(1,16)
ptsize <- 2

#----
#-- plot the ambient treatment
plot(LWP.mean~Date,data=subset(lwp.m.amb,HW_treatment=="control"),ylim=c(-3,0),type="o",las=1,
       pch=21,bg="white",ylab="",xlab="")

#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-5"),format="%Y-%m-%d")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=1,col="darkgrey",density=10) #add rectangles for droughts

#- plot the midday leaf water potentials
adderrorbars(x=lwp.m.amb$Date,y=lwp.m.amb$LWP.mean,SE=lwp.m.amb$LWP.se,direction="updown")
points(LWP.mean~Date,data=subset(lwp.m.amb,HW_treatment=="control"),ylim=c(-3,0),type="o",
     pch=21,bg="white",cex=ptsize)
points(LWP.mean~Date,data=subset(lwp.m.amb,HW_treatment=="heatwave"),ylim=c(-3,0),type="o",
     pch=21,bg="darkgrey",cex=ptsize)

#- add a line for the turgor loss point
abline(h=tlp.m$TLP.mean[1],lwd=2,lty=2)
textxy(X=as.Date("2016-10-27"),Y=tlp.m$TLP.mean[1],lab="Turgor loss point",offset=-1,cex=1)

#- label axes and add a legend
title(main=expression(Ambient~temperature),cex.main=1.5)
title(ylab=expression(Midday~Leaf~Water~Potential~(MPa)),cex.lab=2)
legend("bottomleft",legend=c("Control","Heatwave"),pch=21,col="black",pt.bg=c("white","darkgrey"),pt.cex=ptsize)
box()
legend("topright",legend=letters[1],cex=2,bty="n")


#- add an inset bar chart for Kleaf
plotInset(17095,-1.1,17101,0.15,
          expr={barplot(Kleaf.m$Kleaf.mean[1:2],ylim=c(0,30),col=c("white","darkgrey"),space=0,las=2,cex.axis=1,
                        names.arg=c("Control","Heatwave"),cex.names=1)
            adderrorbars(x=c(0.5,1.5),y=Kleaf.m$Kleaf.mean[1:2],SE=Kleaf.m$Kleaf.se[1:2],direction="updown")
            title(ylab=expression(K[leaf]))
            
          })

#----
#-- plot the warmed treatment
plot(LWP.mean~Date,data=subset(lwp.m.ele,HW_treatment=="control"),ylim=c(-3,0),type="o",
     pch=21,bg="white",ylab="",xlab="")

#- add shaded rectangle for heatwave
dates <- as.Date(c("2016-10-31","2016-11-5"),format="%Y-%m-%d")
rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=1,col="darkgrey",density=10) #add rectangles for droughts

#- plot the midday leaf water potentials
adderrorbars(x=lwp.m.ele$Date,y=lwp.m.ele$LWP.mean,SE=lwp.m.ele$LWP.se,direction="updown")
points(LWP.mean~Date,data=subset(lwp.m.ele,HW_treatment=="control"),ylim=c(-3,0),type="o",
       pch=21,bg="white",cex=ptsize)
points(LWP.mean~Date,data=subset(lwp.m.ele,HW_treatment=="heatwave"),ylim=c(-3,0),type="o",
       pch=21,bg="darkgrey",cex=ptsize)

#- add a line for the turgor loss point
abline(h=tlp.m$TLP.mean[2],lwd=2,lty=2)
textxy(X=as.Date("2016-10-27"),Y=tlp.m$TLP.mean[2],lab="Turgor loss point",offset=-1,cex=1)

#- label axes and add a legend
title(main=expression(Warmed~temperature~(+3*degree*C)),cex.main=1.5)
title(ylab=expression(Midday~Leaf~Water~Potential~(MPa)),cex.lab=2)
legend("bottomleft",legend=c("Control","Heatwave"),pch=21,col="black",pt.bg=c("white","darkgrey"),pt.cex=ptsize)
box()
legend("topright",legend=letters[2],cex=2,bty="n")


#- add an inset bar chart for Kleaf
plotInset(17095,-1.1,17101,0.15,
          expr={barplot(Kleaf.m$Kleaf.mean[3:4],ylim=c(0,30),col=c("white","darkgrey"),space=0,las=2,cex.axis=1,
                        names.arg=c("Control","Heatwave"),cex.names=1)
            adderrorbars(x=c(0.5,1.5),y=Kleaf.m$Kleaf.mean[3:4],SE=Kleaf.m$Kleaf.se[3:4],direction="updown")
            title(ylab=expression(K[leaf]))
            
          })

#----------------------------------------------------------------------------------------------------------