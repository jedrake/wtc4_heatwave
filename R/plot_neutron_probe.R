#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Read and plot the deep soil moisture data
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the data

#- get all the filenames
files <-list.files(path="Data/WTC4_Probe_Data/",pattern="WT")

# loop over each file, read in data
dat.raw1 <- list()
for(i in 1:length(files)){
  dat.temp <- read.table(file=paste("Data/WTC4_Probe_Data/",files[i],sep=""),skip=22,header=T)
  dat.temp <- dat.temp[-which(dat.temp$ID==999),] #get rid of first row
  
  #add "Up.depths" as rownames
  names(dat.temp)[3:15] <- c(475,425,375,325,275,225,175,150,125,100,75,50,25)
  
  #- wide to long format
  names <- names(dat.temp)[4:15]
  dat.long <- reshape(dat.temp, 
             varying = names, 
             v.names = "count",
             timevar = "depth", 
             times = names,
             direction = "long")
  
  #- add a chamber variable
  dat.long$chamber <- as.factor(paste0("C", sprintf("%02.0f", dat.long$ID)))
  
  #- add a date variable from the filename
  dat.long$Date <- as.Date(substr(files[i],start=3,stop=8),format="%d%m%y")
  
  dat.raw1[[i]] <- dat.long[,c("chamber","Date","depth","count")]
}
probedat1 <- do.call(rbind,dat.raw1)
probedat1$depth <- as.numeric(probedat1$depth)

#- merge in the heatwave treatments 
linkdf <- data.frame(chamber = levels(as.factor(wtc$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"),
                     T_treatment=rep(c("ambient","elevated"),6))

probedat1 <- merge(probedat1,linkdf,by="chamber")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- subset to just the heatwave period, plot


probe.hw <- subset(probedat1,Date>as.Date("2016-9-30") & Date < as.Date("2016-11-20"))


#- set up a color palette
colfunc <- colorRampPalette(c("blue", "red"))
#palette(c(colfunc(6),"black"))

#- palette of two blues and two reds
palette(c(brewer.pal(5,"Blues")[3:4],brewer.pal(5,"Reds")[4:5]))


#------------------------------
#- average across treatments, plot
windows(80,80)
par(mfrow=c(2,2),lwd=2,mar=c(0,0,0,0),oma=c(7,9,2,7),cex.axis=1.5)
linewidth=3
xlims <- c(0,450)
ylims=c(1000,16000)
plottype="o"
pointtype=16

dat.d.m <- summaryBy(count~depth+Date+HWtrt+T_treatment,data=probe.hw,FUN=mean,keep.names=T)

#- plot ambient control 
plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="C" & T_treatment=="ambient"),xlim=xlims,ylim=ylims,
       legend=F,type=plottype,lwd=linewidth,pch=pointtype,
       axes=F,xlab="",ylab="")
magaxis(side=c(1,2),labels=c(0,1),frame.plot=T,las=1)
legend("bottomleft",letters[1],bty="n",cex=1.5)
legend("top","Ambient-Control",bty="n")
magaxis(side=c(1,2,4),labels=c(0,0,0),frame.plot=T,las=1)


#- plot ambient heatwave
plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="HW" & T_treatment=="ambient"),xlim=xlims,ylim=ylims,
       legend=F,type=plottype,lwd=linewidth,pch=pointtype,
       axes=F,xlab="",ylab="")
magaxis(side=c(1,2,4),labels=c(0,0,1),frame.plot=T,las=1)
legend("bottomleft",letters[2],bty="n",cex=1.5)
legend("top","Ambient-Heatwave",bty="n")


#- plot warmed control 
plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="C" & T_treatment=="elevated"),xlim=xlims,ylim=ylims,
       legend=F,type=plottype,lwd=linewidth,pch=pointtype,
       axes=F,xlab="",ylab="")
magaxis(side=c(1,2),labels=c(0,1),frame.plot=T,las=1)
legend("bottomleft",letters[3],bty="n",cex=1.5)
legend("top","Warmed-Control",bty="n")
magaxis(side=c(1,2,4),labels=c(1,0,0),frame.plot=T,las=1)


#- plot warmed heatwave
plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="HW" & T_treatment=="elevated"),xlim=xlims,ylim=ylims,
       legend=F,type=plottype,lwd=linewidth,pch=pointtype,
       axes=F,xlab="",ylab="")
magaxis(side=c(1,2,4),labels=c(1,0,1),frame.plot=T,las=1)
legend("bottomleft",letters[4],bty="n",cex=1.5)
legend("top","Warmed-Heatwave",bty="n")


title(xlab="Depth in soil profile (cm)",outer=T,line=3,cex.lab=2.5)
title(ylab="VWC (Neutron probe counts)",outer=T,line=5,cex.lab=2.5)

legend("bottomright",lwd=linewidth,legend=levels(as.factor(probe.hw$Date)),col=palette()[1:5],title="Date")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
