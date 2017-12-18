#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Makes Figure 4- photosynthetic model analysis of whole-canopy flux measurements
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#- Read in the data, do some formatting.
dat.all <- read.csv("Data/WTC_TEMP-PARRA_CM_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv")
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


#--- define the stomatal parameters
# Look at the model predictions for the ambient treatment.  
test <- subset(dat,HWtrt=="C" & PAR >10)

#- fit g1 and g0 for the "test" dataset
gs_fits <- nls(gs ~ g0+1.6*(1+g1/sqrt(VPD))*(Photo/400),start=list(g0=0.002,g1=4),data=subset(test,PAR>500),algorithm="port",
                      lower=c(0,0),upper=c(0.003,10))

#g0 = unname(coef(gs_fits)[1])
g1 = unname(coef(gs_fits)[2])
#g0=0
g1 = 2.9
g0=0.003




#-- fit the control treatment (i.e., not the heatwave trees)
pred1 <- PhotosynEB(Tair=test$Tair_al,VPD=test$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
                                     LeafAbs=0.86,
                                     PPFD=test$PAR,g1=g1,g0=g0,
                                     Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
                                     Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633)
table(pred1$failed) # some energy balance calculations failed
pred1$Tdiff <- with(pred1,Tleaf-Tair)



#-- some test plots
# Plot photosynthesis vs. leaf temperature at high light. 
# 
# plot(Photo~TargTempC_Avg,data=subset(test,PAR>500),pch=3,col="red",ylim=c(0,15),xlim=c(10,45),
# xlab="Tleaf",ylab="Aleaf")
# points(ALEAF~Tleaf,data=subset(pred1,PPFD>500),pch=3)
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
# 
# plot(Trans~TargTempC_Avg,data=subset(test,PAR>500),pch=3,col="red",ylim=c(0,7),xlim=c(10,45),
# xlab="Tleaf",ylab="Eleaf")
# points(ELEAF~Tleaf,data=subset(pred1,PPFD>500),pch=3)
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
# 
# 
# plot(gs~TargTempC_Avg,data=subset(test,PAR>500),pch=3,col="red",ylim=c(0,0.5),xlim=c(10,45))
# points(GS~Tleaf,data=subset(pred1,PPFD>500),pch=3)
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
# 
# # Plot leaf to air temperature difference. 
# 
# plot(Tdiff~PAR,data=test,pch=3,col="red",ylim=c(-2,7))
# points(Tdiff~PPFD,data=subset(pred1,failed==F),pch=3,ylim=c(-2,7))
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
# mean(subset(test,PAR>1000)$Tdiff)
# sd(subset(test,PAR>1000)$Tdiff)
# mean(subset(pred1,PPFD>1000 & failed==F)$Tdiff)
# sd(subset(pred1,PPFD>1000 & failed==F)$Tdiff)
# 
# # Plot predicted value of leaf temperature. 
# plot(pred1$Tleaf~test$TargTempC_Avg);abline(0,1,col="red")




#---------------
# Use the model to predict the data for the heatwave. 
test.hw <- subset(dat,HWtrt=="HW" & PAR >10)
pred2 <- suppressMessages(PhotosynEB(Tair=test.hw$Tair_al,VPD=test.hw$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
                                     LeafAbs=0.86,
                                     PPFD=test.hw$PAR,g1=g1,g0=g0,
                                     Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
                                     Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
table(pred2$failed)
pred2$Tdiff <- with(pred2,Tleaf-Tair)


# # Photosynthesis vs. leaf temperature. 
# plot(Photo~TargTempC_Avg,data=subset(test.hw,PAR>500),pch=3,col="red",ylim=c(0,15),xlim=c(15,45),
# xlab=expression(T[leaf]~(degree*C)),ylab=expression(Photosynthesis~(umol~m^-2~s^-1)),cex.lab=1)
# points(ALEAF~Tleaf,data=subset(pred2,PPFD>500),pch=3)
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"),cex=1)
# 
# 
# # Transpiration vs. leaf temperature. 
# 
# plot(Trans~TargTempC_Avg,data=subset(test.hw,PAR>500),pch=3,col="red",ylim=c(0,5),xlim=c(15,45),
# xlab=expression(T[leaf]~(degree*C)),ylab=expression(Transpiration~(mmol~m^-2~s^-1)),cex.lab=1)
# points(ELEAF~Tleaf,data=subset(pred2,PPFD>500 & failed==F),pch=3)
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"),cex=1)
# 
# 
# # Conductance vs. leaf temperature. 
# plot(GS~Tleaf,data=subset(pred2,PPFD>500),pch=3,ylim=c(0,0.4),xlim=c(10,45))
# points(gs~TargTempC_Avg,data=subset(test.hw,PAR>500),pch=3,col="red")
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
# 
# 
# # Leaf to air temperature difference
# plot(Tdiff~Tair_al,data=test.hw,pch=3,col="red",ylim=c(-2,7))
# points(Tdiff~Tair,data=subset(pred2,failed==F),pch=3,ylim=c(-2,7))
# legend("topleft",c("Model","Data"),pch=3,col=c("black","red"))
# 
# 
# # Predicted value of leaf temperature
# plot(pred2$Tleaf~test.hw$TargTempC_Avg);abline(0,1,col="red")


# What is the error in modeled conductance, transpiration, and leaf temperature (data - model)? 
#Perhaps we can say that leaf temperatures were 2-8 (mean of ~3 degrees C cooler during the heatwave than was predicted by a model.
# gsdiff <-  test.hw$gs-pred2$GS
# plot(gsdiff~test.hw$Tair_al);abline(h=0)
# 
# Ediff <-  test.hw$Trans- pred2$ELEAF
# plot(Ediff~test.hw$Tair_al);abline(h=0)
# 
# test.hw$Tdiff <-  test.hw$TargTempC_Avg- pred2$Tleaf
# plot(test.hw$Tdiff~test.hw$Tair_al);abline(h=0)
# 
# mean(subset(test.hw,Tair_al>40)$Tdiff)
# range(subset(test.hw,Tair_al>40)$Tdiff)












#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#- Make 4-panel density plot, showing A and E for both treatments

#windows(80,70)
pdf("Output/Figure5-Model.pdf")
par(mar=c(0,0,0,0),oma=c(6,6,3,5),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
       widths=c(2,2), heights=c(2,2))
ptsize=0.3
PARlimit=600

#- extract the control data above the PARlimit
testhigh <- subset(test,PAR>PARlimit)
test.hwhigh <- subset(test.hw,PAR>PARlimit)

#- set the color palette
blues.ramp <- colorRampPalette(brewer.pal(9,"Blues")[-1],alpha=0.7)
reds.ramp <- colorRampPalette(brewer.pal(9,"Reds")[-1],alpha=0.7)


#- plot Photosynthesis vs. Tleaf. 
dc <- densCols(testhigh$Photo,testhigh$TargTempC_Avg,colramp=blues.ramp)
plot(testhigh$Photo~testhigh$TargTempC_Avg,col=dc,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n")
points(ALEAF~Tleaf,data=subset(pred1,PPFD>PARlimit),pch=3,cex=ptsize,ylim=c(0,12),xlim=c(15,45))
axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
legend("topright",c("Model","Control data","Heatwave data"),pch=c(3,16,16),col=c("black",dc[1],"red"),cex=1,bg="white")
legend("topleft",legend=paste("(",letters[1],")",sep=""),cex=1.5,bty="n",text.font=2)

dc2 <- densCols(test.hwhigh$Photo,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
plot(test.hwhigh$Photo~test.hwhigh$TargTempC_Avg,col=dc2,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n",
     ylab="",xlab="")
points(ALEAF~Tleaf,data=subset(pred2,PPFD>PARlimit),pch=3,cex=ptsize)
legend("topleft",legend=paste("(",letters[2],")",sep=""),cex=1.5,bty="n",text.font=2)
axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)


#- transiration
dc3 <- densCols(testhigh$Trans,testhigh$TargTempC_Avg,colramp=blues.ramp)
plot(testhigh$Trans~testhigh$TargTempC_Avg,col=dc3,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
points(ELEAF~Tleaf,data=subset(pred1,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
legend("topleft",legend=paste("(",letters[3],")",sep=""),cex=1.5,bty="n",text.font=2)
axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)


dc4 <- densCols(test.hwhigh$Trans,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
plot(test.hwhigh$Trans~test.hwhigh$TargTempC_Avg,col=dc4,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
points(ELEAF~Tleaf,data=subset(pred2,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
legend("topleft",legend=paste("(",letters[4],")",sep=""),cex=1.5,bty="n",text.font=2)
axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)


title(xlab=expression(Canopy~temperature~(degree*C)),outer=T,cex.lab=2,line=4)
title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.95)
title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.05)

dev.off()
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# 
# 
# #---------------------------------------------------------------------------
# #---------------------------------------------------------------------------
# #- plot individual panels for presentation
# windows(20,20)
# ptsize=0.75
# par(mar=c(6,6,1,1),cex.lab=2,las=1,cex.axis=1.1)
# dc2 <- densCols(test.hwhigh$Photo,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
# plot(test.hwhigh$Photo~test.hwhigh$TargTempC_Avg,col=dc2,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n",
#      ylab=expression(Photosynthesis~(mu*mol~m^-2~s^-1)),xlab=expression(T[leaf]~(degree*C)))
# points(ALEAF~Tleaf,data=subset(pred2,PPFD>PARlimit),pch=3,cex=ptsize)
# legend("topright",c("Model","Heatwave data"),pch=c(3,16),col=c("black","red"),cex=1.4,bg="white")
# axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
# 
# dc4 <- densCols(test.hwhigh$Trans,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
# plot(test.hwhigh$Trans~test.hwhigh$TargTempC_Avg,col=dc4,pch=16,cex=1,ylim=c(0,3.5),xlim=c(15,45),xaxt="n",yaxt="n",
#      ylab=expression(Transpiration~(mmol~m^-2~s^-1)),xlab=expression(T[leaf]~(degree*C)))
# points(ELEAF~Tleaf,data=subset(pred2,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
# axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)

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










# #---------------------------------------------------------------------------
# #---------------------------------------------------------------------------
# #- repeat for ball-berry
# 
# pred1BB <- suppressMessages(PhotosynEB(Tair=test$Tair_al,VPD=test$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
#                                      LeafAbs=0.86,
#                                      PPFD=test$PAR,
#                                      gsmodel="BallBerry",g1=9,g0=0.01,
#                                      Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
#                                      Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
# 
# pred2BB <- suppressMessages(PhotosynEB(Tair=test.hw$Tair_al,VPD=test.hw$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
#                                      LeafAbs=0.86,
#                                      PPFD=test.hw$PAR,g1=9,g0=0.01,
#                                      gsmodel="BallBerry",
#                                      Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
#                                      Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
# 
# 
# windows(80,70)
# par(mar=c(0,0,0,0),oma=c(6,6,3,5),cex.lab=1.6,las=1,cex.axis=1.2)
# layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
#        widths=c(2,2), heights=c(2,2))
# ptsize=0.3
# PARlimit=600
# 
# #- extract the control data above the PARlimit
# testhigh <- subset(test,PAR>PARlimit)
# test.hwhigh <- subset(test.hw,PAR>PARlimit)
# 
# #- colors
# blues.ramp <- colorRampPalette(brewer.pal(9,"Blues")[-1],alpha=0.7)
# reds.ramp <- colorRampPalette(brewer.pal(9,"Reds")[-1],alpha=0.7)
# 
# 
# #- plot Photosynthesis vs. Tleaf. 
# dc <- densCols(testhigh$Photo,testhigh$TargTempC_Avg,colramp=blues.ramp)
# plot(testhigh$Photo~testhigh$TargTempC_Avg,col=dc,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n")
# points(ALEAF~Tleaf,data=subset(pred1BB,PPFD>PARlimit),pch=3,cex=ptsize,ylim=c(0,12),xlim=c(15,45))
# axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
# legend("topright",c("Model","Control data","Heatwave data"),pch=c(3,16,16),col=c("black",dc[1],"red"),cex=1.2,bg="white")
# legend("topleft",letters[1],bty="n")
# 
# dc2 <- densCols(test.hwhigh$Photo,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
# plot(test.hwhigh$Photo~test.hwhigh$TargTempC_Avg,col=dc2,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n",
#      ylab="",xlab="")
# points(ALEAF~Tleaf,data=subset(pred2BB,PPFD>PARlimit),pch=3,cex=ptsize)
# legend("topleft",letters[2],bty="n")
# axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)
# 
# 
# #- transiration
# dc3 <- densCols(testhigh$Trans,testhigh$TargTempC_Avg,colramp=blues.ramp)
# plot(testhigh$Trans~testhigh$TargTempC_Avg,col=dc3,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
# points(ELEAF~Tleaf,data=subset(pred1BB,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
# legend("topleft",letters[3],bty="n")
# axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
# 
# 
# dc4 <- densCols(test.hwhigh$Trans,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
# plot(test.hwhigh$Trans~test.hwhigh$TargTempC_Avg,col=dc4,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
# points(ELEAF~Tleaf,data=subset(pred2BB,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
# legend("topleft",letters[4],bty="n")
# axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)
# 
# 
# title(xlab=expression(Canopy~temperature~(degree*C)),outer=T,cex.lab=2,line=4)
# title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.9)
# title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.1)
# 
# #---------------------------------------------------------------------------
# #---------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
# 
# #---------------------------------------------------------------------------
# #---------------------------------------------------------------------------
# #- repeat for Leuning
# 
# pred1Leuning<- suppressMessages(PhotosynEB(Tair=test$Tair_al,VPD=test$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
#                                        LeafAbs=0.86,
#                                        PPFD=test$PAR,
#                                        gsmodel="BBLeuning",g1=7,g0=0.01,
#                                        Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
#                                        Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
# 
# pred2Leuning <- suppressMessages(PhotosynEB(Tair=test.hw$Tair_al,VPD=test.hw$VPD,Wind=8,Wleaf=0.01,StomatalRatio=1,
#                                        LeafAbs=0.86,
#                                        PPFD=test.hw$PAR,g1=7,g0=0.01,
#                                        gsmodel="BBLeuning",
#                                        Vcmax=34,EaV=51780,EdVC=2e5,delsC=640,
#                                        Jmax = 60,EaJ=21640,EdVJ=2e5,delsJ=633))
# 
# 
# windows(80,70)
# par(mar=c(0,0,0,0),oma=c(6,6,3,5),cex.lab=1.6,las=1,cex.axis=1.2)
# layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
#        widths=c(2,2), heights=c(2,2))
# ptsize=0.3
# PARlimit=600
# 
# #- extract the control data above the PARlimit
# testhigh <- subset(test,PAR>PARlimit)
# test.hwhigh <- subset(test.hw,PAR>PARlimit)
# 
# #palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red
# 
# #- colors
# blues.ramp <- colorRampPalette(brewer.pal(9,"Blues")[-1],alpha=0.7)
# reds.ramp <- colorRampPalette(brewer.pal(9,"Reds")[-1],alpha=0.7)
# 
# 
# #- plot Photosynthesis vs. Tleaf. 
# dc <- densCols(testhigh$Photo,testhigh$TargTempC_Avg,colramp=blues.ramp)
# plot(testhigh$Photo~testhigh$TargTempC_Avg,col=dc,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n")
# points(ALEAF~Tleaf,data=subset(pred1Leuning,PPFD>PARlimit),pch=3,cex=ptsize,ylim=c(0,12),xlim=c(15,45))
# axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
# legend("topright",c("Model","Control data","Heatwave data"),pch=c(3,16,16),col=c("black",dc[1],"red"),cex=1.2,bg="white")
# legend("topleft",letters[1],bty="n")
# 
# dc2 <- densCols(test.hwhigh$Photo,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
# plot(test.hwhigh$Photo~test.hwhigh$TargTempC_Avg,col=dc2,pch=16,cex=1,ylim=c(0,12),xlim=c(15,45),xaxt="n",yaxt="n",
#      ylab="",xlab="")
# points(ALEAF~Tleaf,data=subset(pred2Leuning,PPFD>PARlimit),pch=3,cex=ptsize)
# legend("topleft",letters[2],bty="n")
# axis(1,tck=0.025,labels=F);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)
# 
# 
# #- transiration
# dc3 <- densCols(testhigh$Trans,testhigh$TargTempC_Avg,colramp=blues.ramp)
# plot(testhigh$Trans~testhigh$TargTempC_Avg,col=dc3,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
# points(ELEAF~Tleaf,data=subset(pred1Leuning,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
# legend("topleft",letters[3],bty="n")
# axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=T);axis(4,tck=0.025,labels=F)
# 
# 
# dc4 <- densCols(test.hwhigh$Trans,test.hwhigh$TargTempC_Avg,colramp=reds.ramp)
# plot(test.hwhigh$Trans~test.hwhigh$TargTempC_Avg,col=dc4,pch=16,cex=1,ylim=c(0,4),xlim=c(15,45),xaxt="n",yaxt="n")
# points(ELEAF~Tleaf,data=subset(pred2Leuning,PPFD>PARlimit & failed==F),pch=3,cex=ptsize)
# legend("topleft",letters[4],bty="n")
# axis(1,tck=0.025,labels=T);axis(2,tck=0.025,labels=F);axis(4,tck=0.025,labels=T)
# 
# 
# title(xlab=expression(Canopy~temperature~(degree*C)),outer=T,cex.lab=2,line=4)
# title(ylab=expression(A[canopy]~(mu*mol~CO[2]~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.9)
# title(ylab=expression(E[canopy]~(mmol~H[2]*O~m^-2~s^-1)),outer=T,cex.lab=1.5,line=2.5,adj=0.1)
# 
# #---------------------------------------------------------------------------
# #---------------------------------------------------------------------------