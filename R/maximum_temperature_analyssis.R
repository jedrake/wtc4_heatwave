#-----------------------------------------------------------------------------------------------------------
#-  How hot does it get in Richmond?

#- Read in the Richmond RAAF maximum temperature data sets. Do some manipulation and analysis
#  in preparation for out heatwave meeting
#-----------------------------------------------------------------------------------------------------------




#-- load required libraries
library(lubridate)
library(doBy)




#-----------------------------------------------------------------------------------------------------------
#- read in the data (from 1928 through 1994)
dat1 <- read.csv("C:/Repos/wtc4_flux/data/weather_data/RAAF/IDCJAC0010_067033_1800_Data.csv")
dat1$Date <- as.Date(paste(dat1$Year,dat1$Month,dat1$Day,sep=" "),format="%Y %m %d")
names(dat1)[6] <- "maxT"
dat1 <- subset(dat1,dat1$Date >= as.Date("1953-1-1"))[,c("Date","maxT")] # ignore the first few years of data (gaps)

#- read in the second bit of data
dat2 <- read.csv("C:/Repos/wtc4_flux/data/weather_data/RAAF/IDCJAC0010_067105_1800_Data.csv")
dat2$Date <- as.Date(paste(dat2$Year,dat2$Month,dat2$Day,sep=" "),format="%Y %m %d")
names(dat2)[6] <- "maxT"
plot(maxT~Date,data=dat2)

dat2 <- dat2[,c("Date","maxT")]

#- compare overlapping dates for both datasets. data are nearly identical. 
dat3 <- merge(dat1,dat2,by="Date")
plot(maxT.x~maxT.y,data=dat3)
abline(0,1)
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- merge datasets, use the first bit of data up until 1994-10-31
dat2 <- subset(dat2,Date>as.Date("1994-10-30"))[,c("Date","maxT")]

dat <- subset(rbind(dat1,dat2),is.na(maxT)==F)
#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
# plot dataset
windows();par(cex.lab=2,mar=c(6,7,1,1))
plot(maxT~Date,data=subset(dat,Date>as.Date("1998-1-1") & Date < as.Date("2005-1-1")),ylab="Maximum temperature (deg C)")
plot(maxT~Date,data=dat,ylab="Maximum temperature (deg C)")
hist(dat$maxT,freq=F)
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- hottest day on record?
max(dat$maxT)
dat$Date[which.max(dat$maxT)]
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- Mean and max maxT for each day of year
dat$Date_fac <- as.factor(paste(month(dat$Date),day(dat$Date),sep="-"))

dat.m <- summaryBy(maxT~Date_fac,data=dat,FUN=c(mean,max))
dat.m$Date <- as.Date(paste("2010",dat.m$Date_fac,sep="-"),format="%Y-%m-%d")
plot(maxT.mean~Date,data=dat.m,ylim=c(10,50),ylab="Maximum temperature (deg C)")
points(maxT.max~Date,data=dat.m,pch=16)
legend("top",pch=c(16,1),c("Max-maxT","Mean-maxT"))
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- Get quantiles
dat.l <- split(dat,dat$Date_fac)
dat.ql <- list()
for(i in 1:length(dat.l)){
  quantiles <- quantile(dat.l[[i]]$maxT,probs=c(0.9,0.95,0.98))
  dat.ql[[i]] <- data.frame(q90=unname(quantiles[1]),q95=unname(quantiles[2]),q98=unname(quantiles[3]))
  dat.ql[[i]]$Date_fac <- as.character(dat.l[[i]]$Date_fac[1])
}
dat.q <- as.data.frame(do.call(rbind,dat.ql))
dat.q$Date <- as.Date(paste("2010",dat.q$Date_fac,sep="-"),format="%Y-%m-%d")
names(dat.q)[1:3] <- c("q90","q95","q98")

dat.q <- dat.q[with(dat.q,order(Date)),]

#- plot quantiles for all days
plot(q98~Date,data=dat.q,ylab="maxT quantiles",lty=1,col="red",type="l")
points(q95~Date,data=dat.q,ylab="maxT quantiles",lty=1,col="forestgreen",type="l")
points(q90~Date,data=dat.q,ylab="maxT quantiles",lty=1,col="blue",type="l")
legend("top",lty=c(1,1,1),c("98%","95%","90%"),col=c("red","forestgreen","blue"))


#- plot quantiles for November and December only
plot(q98~Date,data=subset(dat.q,Date>=as.Date("2010-11-1")),ylab="maxT quantiles",lty=1,col="red",type="l",ylim=c(30,45))
points(q95~Date,data=dat.q,ylab="maxT quantiles",lty=1,col="forestgreen",type="l")
points(q90~Date,data=dat.q,ylab="maxT quantiles",lty=1,col="blue",type="l")
legend("top",lty=c(1,1,1),c("98%","95%","90%"),col=c("red","forestgreen","blue"))
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
# How many observations are there, with 5 days consistently > 40 degrees max T?
library(dplyr)
#library(devtools)
#install_github("kevinushey/RcppRoll")
library(RcppRoll)

dat$maxT_flag <- 0
dat$maxT_flag[which(dat$maxT>35)] <- 1

#- calculate the 5-day rolling sum of flag data
out <- mutate(dat, roll_sum(dat$maxT_flag, 5, fill=0))
names(out)[5] <- "nmax_flag"

#- how many extreme events?
length(which(out$nmax_flag==2))

#- look at the crazy hot period in early Feb, 2009
dat[which(out$nmax_flag==4),]

datemin <- as.Date("2009-02-2")
datemax <- as.Date("2009-02-12")
plot(maxT~Date,data=subset(dat,Date>datemin & Date < datemax),type="o",ylim=c(20,45))


#- calculate the 5-day rolling average of maxT
out2 <- mutate(dat, roll_mean(dat$maxT, 5, fill=0))
names(out2)[5] <- "maxT_mean"

datemin <- as.Date("1979-01-1")
datemax <- as.Date("1979-01-14")
plot(maxT~Date,data=subset(dat,Date>datemin & Date < datemax),type="o",ylim=c(20,45))



datemin <- as.Date("1972-12-17")
datemax <- as.Date("1972-12-27")
plot(maxT~Date,data=subset(dat,Date>datemin & Date < datemax),type="o",ylim=c(20,45))


datemin <- as.Date("2011-01-25")
datemax <- as.Date("2011-02-10")
plot(maxT~Date,data=subset(dat,Date>datemin & Date < datemax),type="o",ylim=c(20,45))


datemin <- as.Date("2013-01-01")
datemax <- as.Date("2013-01-30")
plot(maxT~Date,data=subset(dat,Date>datemin & Date < datemax),type="o",ylim=c(20,45))

#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- identify the events with >3 days of Tmax >90th percentile for each DOY

#- put the 90th quantile in with the daily observations (dat)
dat2 <- merge(dat,dat.q[,c("Date_fac","q90")],by="Date_fac")
dat2$hot <- ifelse(dat2$maxT>dat2$q90,1,0)
dat2 <- dat2[with(dat2,order(Date)),]

library(dplyr)
library(RcppRoll)

#- calculate the 5-day rolling sum of flag data
out2 <- dplyr::mutate(dat2, roll_sum(dat2$hot, 3, fill=0))
names(out2)[6] <- "nmax_flag"

#- subset to data that would be included in the precip record
out3 <- subset(out2,Date<=as.Date("2015-02-28"))

#- how many extreme events?
length(which(out3$nmax_flag==3))

#- dates with 3 or more consecutive hot days
hotdates <- out3$Date[which(out3$nmax_flag==3)]
#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
#- read in the precip record
precip <- read.csv("Data/BOM_precip_RAAF.csv")
precip$Date <- paste(precip$Year,precip$Month,precip$Day,sep=" ")
precip$Date <- as.Date(precip$Date,format="%Y %m %d")
names(precip)[5] <- "Rain_mm"
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- loop over the hot dates, find associated precip events, and sum them
precipsums <- c()
for (i in 1:length(hotdates)){
  focaldate <- hotdates[i]
  
  #- establish a vector of dates preceding and including the hot date
  searchdates <- seq(focaldate-14,focaldate,by=1)
  
  #- find matching precip records, sum them
  precipevents <- which(precip$Date %in% searchdates)
  precipsums[i] <- sum(precip$Rain_mm[precipevents],na.rm=T)
  
}
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- look at results
windows()
par(mar=c(6,6,2,1))
hist(precipsums,breaks=30,xlab="Precip in 14 prior days (mm)",cex.lab=2,
     main="Events, 3 days > 90th percentile for Tmax")


#- 82% of heatwaves took place with less than 30mm rain in the preceding 14 days
length(which(precipsums<30))/length(precipsums)

#- 44% of heatwaves took place with less than 5mm rain in the preceding 14 days
length(which(precipsums<5))/length(precipsums)

#- 10% of heatwaves took place with zero rain in the preceding 14 days
length(which(precipsums==0))/length(precipsums)
#-----------------------------------------------------------------------------------------------------------