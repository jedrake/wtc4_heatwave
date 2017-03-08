## "WTC4 heatwave sandbox. Can we use a leaf-scale model that includes an energy balance 
##  to predict the temperature leaves would have been during the heatwave in the absence of latent cooling?"

source("R/loadLibraries.R")

## Look at the implications of photosynthetic decoupling in the WTC4 heatwave with PlantecophysEB. 
# Assumes that HIEv and plantecophys are installed, loaded, and the HIEv token is set. Get the data. If a "Data" directory doesn't exist, make one. Download the data from hiev, read it in, do some formatting.

if(!dir.exists("Data")) dir.create(file.path("Data"),showWarnings=F)
downloadHIEv(searchHIEv("WTC_TEMP-PARRA_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv"),topath="Data",quiet=T)
dat.all <- read.csv("Data/WTC_TEMP-PARRA_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv")
dat.all$DateTime_hr <- as.POSIXct(dat.all$DateTime_hr,format="%Y-%m-%d %T",tz="GMT")
dat.all$Tdiff <- with(dat.all,TargTempC_Avg-Tair_al)

# Calculate conductance as the simple ratio between Trans and VPD. But use leaf to air VPD. Leaf to air VPD tends to be greater than air VPD, as leaves tend to be warmer than air. Plot the relationships between photosynthesis, conductance, and leaftoairVPD. Note the shift in points at extreme temperatures and VPD in teh heatwave treatment, consistent with lower photosynthesis than would be expected given the measured gs.
dat.all$leaftoairVPD <- VPDairToLeaf(VPD=dat.all$VPD,Tair=dat.all$Tair_al,Tleaf=dat.all$TargTempC_Avg)
dat.all$gs <- dat.all$Trans/dat.all$leaftoairVPD/10
dat.all$Atogs <- with(dat.all,Photo/(gs*1000))

#- aggregate to hourly averages
dat.all$DateTime_hr <- nearestTimeStep(dat.all$DateTime_hr,nminutes=30,"floor")
dat.hr <- summaryBy(.~DateTime_hr+chamber+T_treatment+HWtrt+combotrt,FUN=mean,keep.names=T,data=dat.all)

#- subset to just the heatwave timeperiods
starttime <- as.POSIXct("2016-10-20 00:00:00",format="%Y-%m-%d %T",tz="GMT")
endtime <- as.POSIXct("2016-11-11 20:00:00",format="%Y-%m-%d %T",tz="GMT")
dat <- subset(dat.hr,DateTime_hr>starttime & DateTime_hr<endtime)

# plotBy(Photo~leaftoairVPD|HWtrt,data=subset(dat,PAR>500),col=c("blue","red"))
# plotBy(gs~leaftoairVPD|HWtrt,data=subset(dat,PAR>500),col=c("blue","red"))
# plotBy(Atogs~leaftoairVPD|HWtrt,data=subset(dat,PAR>500),col=c("blue","red"),ylim=c(0,0.5))
# plotBy(Photo~gs|HWtrt,data=subset(dat,PAR>500),col=c("blue","red"))


# Look at the model predictions for the ambient treatment.  

test <- subset(dat,HWtrt=="C" & PAR >10)
g0 <- 0.001
pred1 <- suppressMessages(PhotosynEB(Tair=test$Tair_al,VPD=test$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
                                     LeafAbs=0.86,
                                     PPFD=test$PAR,g1=3,g0=g0,
                                     Vcmax=35,EaV=51780,EdVC=2e5,delsC=645,
                                     Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
table(pred1$failed)
pred1$Tdiff <- with(pred1,Tleaf-Tair)

# Plot photosynthesis vs. leaf temperature at high light. 

plot(Photo~TargTempC_Avg,data=subset(test,PAR>500),pch=3,col="red",ylim=c(0,15),xlim=c(10,45),
xlab="Tleaf",ylab="Aleaf")
points(ALEAF~Tleaf,data=subset(pred1,PPFD>500),pch=3)
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))

plot(Trans~TargTempC_Avg,data=subset(test,PAR>500),pch=3,col="red",ylim=c(0,7),xlim=c(10,45),
xlab="Tleaf",ylab="Eleaf")
points(ELEAF~Tleaf,data=subset(pred1,PPFD>500),pch=3)
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))


plot(gs~TargTempC_Avg,data=subset(test,PAR>500),pch=3,col="red",ylim=c(0,0.5),xlim=c(10,45))
points(GS~Tleaf,data=subset(pred1,PPFD>500),pch=3)
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))

# Plot leaf to air temperature difference. 

plot(Tdiff~PAR,data=test,pch=3,col="red",ylim=c(-2,7))
points(Tdiff~PPFD,data=subset(pred1,failed==F),pch=3,ylim=c(-2,7))
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
mean(subset(test,PAR>1000)$Tdiff)
sd(subset(test,PAR>1000)$Tdiff)
mean(subset(pred1,PPFD>1000 & failed==F)$Tdiff)
sd(subset(pred1,PPFD>1000 & failed==F)$Tdiff)

# Plot predicted value of leaf temperature. 
plot(pred1$Tleaf~test$TargTempC_Avg);abline(0,1,col="red")




#---------------
# Use the model to predict the data for the heatwave. 
test.hw <- subset(dat,HWtrt=="HW" & PAR >10)
pred2 <- suppressMessages(PhotosynEB(Tair=test.hw$Tair_al,VPD=test.hw$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
                                     LeafAbs=0.86,
                                     PPFD=test.hw$PAR,g1=3,g0=g0,
                                     Vcmax=35,EaV=51780,EdVC=2e5,delsC=645,
                                     Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
table(pred2$failed)
pred2$Tdiff <- with(pred2,Tleaf-Tair)


# Photosynthesis vs. leaf temperature. 
plot(Photo~TargTempC_Avg,data=subset(test.hw,PAR>500),pch=3,col="red",ylim=c(0,15),xlim=c(15,45),
xlab=expression(T[leaf]~(degree*C)),ylab=expression(Photosynthesis~(umol~m^-2~s^-1)),cex.lab=1)
points(ALEAF~Tleaf,data=subset(pred2,PPFD>500),pch=3)
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"),cex=1)


# Transpiration vs. leaf temperature. 

plot(Trans~TargTempC_Avg,data=subset(test.hw,PAR>500),pch=3,col="red",ylim=c(0,5),xlim=c(15,45),
xlab=expression(T[leaf]~(degree*C)),ylab=expression(Transpiration~(mmol~m^-2~s^-1)),cex.lab=1)
points(ELEAF~Tleaf,data=subset(pred2,PPFD>500 & failed==F),pch=3)
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"),cex=1)


# Conductance vs. leaf temperature. 
plot(GS~Tleaf,data=subset(pred2,PPFD>500),pch=3,ylim=c(0,0.4),xlim=c(10,45))
points(gs~TargTempC_Avg,data=subset(test.hw,PAR>500),pch=3,col="red")
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))


# Leaf to air temperature difference
plot(Tdiff~Tair_al,data=test.hw,pch=3,col="red",ylim=c(-2,7))
points(Tdiff~Tair,data=subset(pred2,failed==F),pch=3,ylim=c(-2,7))
legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))


# Predicted value of leaf temperature
plot(pred2$Tleaf~test.hw$TargTempC_Avg);abline(0,1,col="red")


# What is the error in modeled conductance, transpiration, and leaf temperature (data - model)? 
#Perhaps we can say that leaf temperatures were 2-8 (mean of ~3 degrees C cooler during the heatwave than was predicted by a model.
gsdiff <-  test.hw$gs-pred2$GS
plot(gsdiff~test.hw$Tair_al);abline(h=0)

Ediff <-  test.hw$Trans- pred2$ELEAF
plot(Ediff~test.hw$Tair_al);abline(h=0)

test.hw$Tdiff <-  test.hw$TargTempC_Avg- pred2$Tleaf
plot(test.hw$Tdiff~test.hw$Tair_al);abline(h=0)

mean(subset(test.hw,Tair_al>40)$Tdiff)
range(subset(test.hw,Tair_al>40)$Tdiff)





#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#- Make a nice 4-panel plot, showing A and E for both treatments

windows(80,70)
par(mar=c(2,2,1,2),oma=c(4,5,2,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
       widths=c(2,2), heights=c(2,2))
ptsize=0.7
PARlimit=600
#palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red

#- plot Photosynthesis vs. Tleaf. 
plot(Photo~TargTempC_Avg,data=subset(test,PAR>PARlimit),pch=3,col="blue",ylim=c(0,12),xlim=c(15,45),
     xlab="",ylab="",cex=ptsize)
points(ALEAF~Tleaf,data=subset(pred1,PPFD>PARlimit),pch=3,cex=ptsize)
legend("topright",c("Model","Control data","Heatwave data"),pch=3,col=c("black","blue","red"),cex=1.2)
legend("topleft",letters[1],bty="n")

plot(Photo~TargTempC_Avg,data=subset(test.hw,PAR>PARlimit),pch=3,col="red",ylim=c(0,12),xlim=c(15,45),
     xlab="",ylab="",cex=ptsize)
points(ALEAF~Tleaf,data=subset(pred2,PPFD>PARlimit),pch=3,cex=ptsize)
legend("topleft",letters[2],bty="n")

plot(Trans~TargTempC_Avg,data=subset(test,PAR>PARlimit),pch=3,col="blue",ylim=c(0,4),xlim=c(15,45),cex=ptsize,
     xlab=expression(T[leaf]~(degree*C)),ylab=expression(Transpiration~(mmol~m^-2~s^-1)),cex.lab=1)
points(ELEAF~Tleaf,data=subset(pred1,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
legend("topleft",letters[3],bty="n")

#- transiration
plot(Trans~TargTempC_Avg,data=subset(test.hw,PAR>PARlimit),pch=3,col="red",ylim=c(0,4),xlim=c(15,45),cex=ptsize,
     xlab=expression(T[leaf]~(degree*C)),ylab=expression(Transpiration~(mmol~m^-2~s^-1)),cex.lab=1)
points(ELEAF~Tleaf,data=subset(pred2,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
legend("topleft",letters[4],bty="n")

title(xlab=expression(Leaf~temperature~(T[leaf]~degree*C)),outer=T,cex.lab=1.5,line=1.5)
title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,cex.lab=1.5,line=1.5,adj=0.9)
title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,cex.lab=1.5,line=1.5,adj=0.1)

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------











#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#- Take another shot at the 4-panel plot, showing A and E for both treatments
#  But try and do a DENSITY plot this time.

windows(80,70)
par(mar=c(0,0,0,0),oma=c(6,6,3,5),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
       widths=c(2,2), heights=c(2,2))
ptsize=0.3
PARlimit=600

#- extract the control data above the PARlimit
testhigh <- subset(test,PAR>PARlimit)
test.hwhigh <- subset(test.hw,PAR>PARlimit)

#palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red

#- install the geneplotter package to make density plots
## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("geneplotter")
library(geneplotter); library(RColorBrewer)
blues.ramp <- colorRampPalette(brewer.pal(9,"Blues")[-1],alpha=0.7)
reds.ramp <- colorRampPalette(brewer.pal(9,"Reds")[-1],alpha=0.7)


#- plot Photosynthesis vs. Tleaf. 
dc <- densCols(testhigh$Photo,testhigh$TargTempC_Avg,colramp=blues.ramp)
plot(testhigh$Photo~testhigh$TargTempC_Avg,col=dc,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n")
points(ALEAF~Tleaf,data=subset(pred1,PPFD>PARlimit),pch=3,cex=ptsize,ylim=c(0,12),xlim=c(15,45))
axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
legend("topright",c("Model","Control data","Heatwave data"),pch=c(3,16,16),col=c("black",dc[1],"red"),cex=1.2,bg="white")
legend("topleft",letters[1],bty="n")

dc2 <- densCols(test.hwhigh$Photo,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
plot(test.hwhigh$Photo~test.hwhigh$TargTempC_Avg,col=dc2,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n")
points(ALEAF~Tleaf,data=subset(pred2,PPFD>PARlimit),pch=3,cex=ptsize)
legend("topleft",letters[2],bty="n")
axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)


#- transiration
dc3 <- densCols(testhigh$Trans,testhigh$TargTempC_Avg,colramp=blues.ramp)
plot(testhigh$Trans~testhigh$TargTempC_Avg,col=dc3,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
points(ELEAF~Tleaf,data=subset(pred1,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
legend("topleft",letters[3],bty="n")
axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)


dc4 <- densCols(test.hwhigh$Trans,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
plot(test.hwhigh$Trans~test.hwhigh$TargTempC_Avg,col=dc4,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
points(ELEAF~Tleaf,data=subset(pred2,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
legend("topleft",letters[4],bty="n")
axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)


title(xlab=expression(Leaf~temperature~(T[leaf]~degree*C)),outer=T,cex.lab=2,line=4)
title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.9)
title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.1)

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#- extract RMSE and r2 values for the Acanopy and Ecanopy fits.
lm.A <- lm(test$Photo~pred1$ALEAF)
summary(lm.A)$sigma #RMSE
summary(lm.A)$r.squared #r2

lm.E <- lm(test$Trans~pred1$ELEAF)
summary(lm.E)$sigma #RMSE
summary(lm.E)$r.squared #r2

lm.leafT <- lm(test$TargTempC_Avg~pred1$Tleaf)
summary(lm.leafT)$sigma #RMSE
summary(lm.leafT)$r.squared #r2
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------