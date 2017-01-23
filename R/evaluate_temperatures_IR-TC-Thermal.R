#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Assess the three kinds of leaf temperature data during the test campaigns in June and September.
#  Thermal photos were taken on 27th June (lower canopy) and again on 21 Sept (upper canopy)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

source("R/loadLibraries.R")



#-----------------------------------------------------------------------------------------------------------
#- download the leaf temperature data from HIEv (i.e., the actual thermocouples!)
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_LEAFTEMPS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{2}_LEAFTEMPS", startDate="2016-06-01", endDate="2016-09-30",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_Tleaf.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
d$DateTime_hr <- nearestTimeStep(d$DateTime, nminutes = 15, align = "floor")

#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- download the AIRVARS data (PPFD and IR-T data), do some manipulation
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_AIRVARS")
IRT <- downloadTOA5("WTC_AUTO_C[0-9]{2}_AIRVARS", startDate="2016-6-1", endDate="2016-09-30",
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
Trend.dat <- subset(TrendlogChDF,as.Date(DateTime)>=as.Date("2016-6-1") & as.Date(DateTime)<=as.Date("2016-9-30"))
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
dat.raw <- merge(dat2,Trend.dat.m,by=c("chamber","DateTime_hr"))

dat.raw$Tleaf <- rowMeans(dat.raw[,c("LeafT_Avg.1.","LeafT_Avg.2.")],na.rm=T)
#-----------------------------------------------------------------------------------------------------------










#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the leaf light dataset

files <- list.files(path="c:/Repos/WTC4_flux/data/leafLight/",pattern="LEAFLIGHT_R.dat",full.names=TRUE)

#- remove the backup files, and the files from the first campaign
files <- files[-grep("backup",files)]
files <- files[-grep("CAMPAIGN1",files)]


ll.l <- list()
for(i in 1:length(files)){
  ll.l[[i]] <- readTOA5(files[i])
  ll.l[[i]]$chamber <- substr(files[i],start=44,stop=46)
}
ll <- do.call(rbind,ll.l)
ll2 <- subset(ll,DateTime > as.POSIXct("2016-06-28 11:50:00",tz="UTC"))[,c("DateTime","GASP_Avg.1.","GASP_Avg.2.","chamber")]

#- apply calibrations for each GASP
cals <- data.frame(chamber=c("C01","C01","C10","C10"),sensor=c(1,2,1,2),
                   int=rep(0,4),
                   slope = c(483.45,505.70,780.74,530.2),
                   curve = c(-19.3,-7,-114.71,-22.7))

#- apply the GASP cals
ll2$GASP_PAR_2 <- ll2$GASP_PAR_1 <- NA
C01inds <- which(ll2$chamber=="C01")
C10inds <- which(ll2$chamber=="C10")

ll2$GASP_PAR_1[C01inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.1.[C01inds]+cals[1,5]*ll2$GASP_Avg.1.[C01inds]
ll2$GASP_PAR_2[C01inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.2.[C01inds]+cals[1,5]*ll2$GASP_Avg.2.[C01inds]
ll2$GASP_PAR_1[C10inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.1.[C10inds]+cals[1,5]*ll2$GASP_Avg.1.[C10inds]
ll2$GASP_PAR_2[C10inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.2.[C10inds]+cals[1,5]*ll2$GASP_Avg.2.[C10inds]

#- first campaign was from 2016-06-28 to 2016-07-14. Second campaign was from 2016-09-20 to 2016-10-06
c1 <- subset(ll2,DateTime> as.POSIXct("2016-06-28 00:00:00",tz="UTC") & DateTime < as.POSIXct("2016-07-14 23:59:00",tz="UTC"))
c1$campaign <- 1

c2 <- subset(ll2,DateTime> as.POSIXct("2016-09-20 00:00:00",tz="UTC") & DateTime < as.POSIXct("2016-10-06 23:59:00",tz="UTC"))
c2$campaign <- 2

ll3 <- rbind(c1,c2)

#-----------------------------------------------------------------------------------------------------------
#- merge the within-chamber met data with the leaf light dataset
#d5 <- merge(TrendlogChDF,ll3,by=c("chamber","DateTime"),all.x=F) # can't allocate vector of 105 MB
d5 <- merge(subset(TrendlogChDF,as.Date(DateTime)>=as.Date("2016-6-1") & as.Date(DateTime)<=as.Date("2016-9-30")),
            ll3,by=c("chamber","DateTime"),all.x=F)

#- merge in the leaf temperatures
dat.ll <- merge(d5,d,by=c("chamber","DateTime"))
dat.ll$DateTime <- as.POSIXct(dat.ll$DateTime,tz="GMT")
dat.ll$chamber <- factor(dat.ll$chamber)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- leaf temperatures respond strongly to incident light
  
#- There was a leaf gas exchange campaign on 27/09/2016, so Tair would have been held at 25 deg C for several hours
#- subset to just the afternoon when the setpoint as 25C
# there was also a campaign on 6/07/2016
dat.ll.sub <- subset(dat.ll,DateTime >= as.POSIXct("2016-09-27 16:00:00",tz="GMT",format="%F %T") & DateTime <= as.POSIXct("2016-09-27 17:00:00",tz="GMT",format="%F %T"))

plotBy(GASP_PAR_2~DateTime|chamber,data=dat.ll.sub,type="l",legend=F)

#- plot the first thermocouple in chamber 10
windows()
par(las=1,mar=c(6,6,1,6),cex.lab=1.5,cex.axis=1.1)
#- plot leaf temperature
plot(LeafT_Avg.1.~DateTime,data=subset(dat.ll.sub,chamber=="C10"),type="b",col="black",ylim=c(22,28),xaxt="n",
     xlab="Time",
     ylab=expression(Temperature~(degree*C)))
text(x=dat.ll.sub$DateTime[10],y=26,expression(T[leaf]))


#add air temperature
lines(Tair_al~DateTime,data=subset(dat.ll.sub,chamber=="C10"),lty=3,lwd=3,col="darkgrey")
text(x=dat.ll.sub$DateTime[10],y=24,expression(T[air]))

#- add incident light
par(new = T)
plot(GASP_PAR_1~DateTime,data=subset(dat.ll.sub,chamber=="C10"),col="blue",type="l",axes=F,ylim=c(0,2000),
     ylab="",xlab="")
text(x=dat.ll.sub$DateTime[5],y=1750,expression(PPFD),col="blue")

axis(side=4)
title(ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),xpd=NA,line=-28)

#- add x-axis 
axis.POSIXct(side=1,x=subset(dat.ll.sub,chamber=="C10")$DateTime,format="%H:%M")


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- comparison plots of IR and TC data


#- subset to just the two "campaigns". The first campaign started on 28 June 2016. The second campaign started on 20 Sept.
#  both campaigns lasted for a week

#- according to the leaf light notes, 
#- first campaign was from 2016-06-28 to 2016-07-14. Second campaign was from 2016-09-20 to 2016-10-06
#  Thermal photos were taken on 27th June (lower canopy) and again on 21 Sept (upper canopy)

Tdat1 <- subset(dat.raw,DateTime_hr> as.POSIXct("2016-06-26 00:00:00",tz="UTC") & DateTime_hr < as.POSIXct("2016-07-14 23:59:00",tz="UTC"))
Tdat2 <- subset(dat.raw,DateTime_hr> as.POSIXct("2016-09-20 00:00:00",tz="UTC") & DateTime_hr < as.POSIXct("2016-10-06 23:59:00",tz="UTC"))

Tdat <- rbind(Tdat1,Tdat2)
Tdat$Tdiff_TC1 <- with(Tdat,LeafT_Avg.1.-Tair_al)
Tdat$Tdiff_TC2 <- with(Tdat,LeafT_Avg.2.-Tair_al)

#- NA-fill some crazy values
tonafill1 <- which(Tdat$Tdiff_TC1 < -4 | Tdat$Tdiff_TC1 > 12)
tonafill2 <- which(Tdat$Tdiff_TC2 < -4 | Tdat$Tdiff_TC2 > 12)
Tdat[tonafill1,c("LeafT_Avg.1.","Tdiff_TC1")] <- NA
Tdat[tonafill2,c("LeafT_Avg.2.","Tdiff_TC2")] <- NA


#- calculate average TC temperature
Tdat$Tleaf <- rowMeans(Tdat[,c("LeafT_Avg.1.","LeafT_Avg.2.")])
Tdat$Tdiff_TC <- with(Tdat,Tleaf-Tair_al)
Tdat$Tdiff_IR <- with(Tdat,TargTempC_Avg-Tair_al)


#-- read in the flux data and merge (to get PAR at the top of the demountable)
wtc1 <- read.csv("c:/Repos/wtc4_flux_processing/output/WTC_TEMP-PARRA_WTCFLUX_20160228-20161123_L0.csv")
wtc2 <- wtc1[,c("chamber","DateTime","PAR")]
wtc2$DateTime <- as.POSIXct(wtc2$DateTime,format="%Y-%m-%d %T",tz="GMT")
wtc2$DateTime_hr <- nearestTimeStep(wtc2$DateTime, nminutes = 15, align = "floor")
wtc <- data.frame(dplyr::summarize(dplyr::group_by(wtc2, DateTime_hr, chamber), 
                                         PAR=mean(PAR,na.rm=T)))

Tdat <- merge(Tdat,wtc,by=c("chamber","DateTime_hr"),all.x=T)

Tdat$DateTime_hr2 <- nearestTimeStep(Tdat$DateTime_hr, nminutes = 60, align = "floor")
Tdat_hr <- summaryBy(.~chamber+DateTime_hr2,data=Tdat,keep.names=T)

#- plot leaf temperatures relative to air temperatures
windows(100,100)
par(mfrow=c(2,2),las=1,cex.lab=1.5,mar=c(6,6,1,1))
lims <- c(0,40)
parlimit=0

#- plot leaf temperatures relative to air temperature
plot(TargTempC_Avg~Tair_al,data=subset(Tdat_hr,PAR>parlimit),ylim=lims,xlim=lims,pch=3,
     xlab=expression(T[air]~(degree*C)),
     ylab=expression(T[l-IR]));abline(0,1,col="grey",lty=2)
lm1 <- lm(TargTempC_Avg~Tair_al,data=subset(Tdat_hr,PAR>parlimit))
lines(predict.lm(lm1,newdat=data.frame(Tair_al=seq(0,35,length.out=101))) ~ seq(0,35,length.out=101),lwd=3,col="grey")
legend("topleft",legend=letters[1],cex=1.4,bty="n")

plot(LeafT_Avg.1.~Tair_al,data=subset(Tdat_hr,PAR>parlimit),ylim=lims,xlim=lims,pch=3,
     xlab=expression(T[air]~(degree*C)),
     ylab=expression(T[l-TC]));abline(0,1,col="grey",lty=2)
points(LeafT_Avg.2.~Tair_al,data=subset(Tdat_hr,PAR>parlimit),pch=3)
lm2 <- lm(Tleaf~Tair_al,data=subset(Tdat_hr,PAR>parlimit))
lines(predict.lm(lm2,newdat=data.frame(Tair_al=seq(0,35,length.out=101))) ~ seq(0,35,length.out=101),lwd=3,col="grey")
legend("topleft",legend=letters[2],cex=1.4,bty="n")


#- plot covariance between temperature measurements
plot(Tleaf~TargTempC_Avg,data=subset(Tdat_hr,PAR>parlimit),ylim=lims,xlim=lims,pch=3,
     xlab=expression(T[l-IR]~(degree*C)),
     ylab=expression(T[l-TC]));abline(0,1,col="grey",lty=2)
lm3 <- lm(Tleaf~TargTempC_Avg,data=subset(Tdat_hr,PAR>parlimit))
lines(predict.lm(lm3,newdat=data.frame(TargTempC_Avg=seq(0,35,length.out=101))) ~ seq(0,35,length.out=101),lwd=3,col="grey")
confint(lm3)
legend("topleft",legend=letters[3],cex=1.4,bty="n")

#- plot the excursion between Tleaf and Tair as a function of PPFD
plot(Tdiff_IR~PAR,data=subset(Tdat_hr,PAR>4),ylim=c(-5,12),xlim=c(0,2000),pch=3,col="black",
     xlab=expression(PPFD~(mu*mol~m^-2~s^-1)),
     ylab=expression(T[leaf]-T[air]~(degree*C)));abline(h=0)
points(Tdiff_TC~PAR,data=Tdat_hr,pch=3,col="blue")
legend("bottomright",fill=c("black","blue","red"),legend=c("Infrared","Thermocouple","Thermal images"))
lm4 <- lm(Tdiff_TC~PAR,data=Tdat_hr)
predicts.Tdiff <- predict.lm(lm4,newdat=data.frame(PAR=seq(0,1600,length.out=101)),interval="prediction")
lines(predicts.Tdiff[,1]~ seq(0,1600,length.out=101),lwd=3,col="grey")
lines(predicts.Tdiff[,2]~ seq(0,1600,length.out=101),lwd=2,lty=2,col="grey")
lines(predicts.Tdiff[,3]~ seq(0,1600,length.out=101),lwd=2,lty=2,col="grey")
legend("topleft",legend=letters[4],cex=1.4,bty="n")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- compare thermal camera data to others, add Tdiff to plot


#- get the thermal camera data
TCdat1 <- read.csv("Data/WTC4_TLEAF_THERMALCAM_20160629.csv")
TCdat2 <- read.csv("Data/WTC4_TLEAF_THERMALCAM_20160921.csv")
TCdat <- rbind(TCdat1,TCdat2[,1:6])
TCdat$Date <- as.Date(TCdat$Date)
names(TCdat)[5] <- "Tleaf_Thermo"
TCdat$DateTime <- as.POSIXct(paste(TCdat$Date,TCdat$Time,sep=" "),format="%F %R",tz="GMT")
TCdat$DateTime_hr2 <- nearestTimeStep(TCdat$DateTime, nminutes = 60, align = "floor")

#- merge thermal camera data in with IR and TC data. Note that IR and TC data will be duplicated.
TCdat.all <- merge(TCdat,Tdat,by=c("chamber","DateTime_hr2"))
TCdat.all$Tdiff_Thermo <- with(TCdat.all,Tleaf_Thermo-Tair_al)

points(Tdiff_Thermo~PAR,data=TCdat.all,pch=16,col="red")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
