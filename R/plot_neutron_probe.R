#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Read and plot the deep soil moisture data
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
source("R/loadLibraries.R")



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
linkdf <- data.frame(chamber = levels(as.factor(probedat1$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"),
                     T_treatment=rep(c("ambient","elevated"),6))

probedat1 <- merge(probedat1,linkdf,by="chamber")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate the proportional water uptake for each depth, comparing the measurements prior to the 
#  heatwave (on 2016-10-28) and at teh end of teh heatwave (on 2016-11-04)

# subset to the two relevant dates
probedat.pre <- subset(probedat1,Date==as.Date("2016-10-28"))
names(probedat.pre)[4] <- "count.pre"
probedat.post <- subset(probedat1,Date==as.Date("2016-11-04"))
names(probedat.post)[4] <- "count.post"

# merge, calculate the difference, and average
probedat.diff <- merge(probedat.pre,probedat.post,by=c("chamber","depth","HWtrt","T_treatment"))
probedat.diff$count.diff <- with(probedat.diff,count.pre-count.post)

probedat.diff.m <- summaryBy(count.diff~depth+HWtrt,data=probedat.diff,FUN=c(mean,se),keep.names=F)

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
windows(140,80)
par(mfrow=c(1,3),lwd=2,mar=c(7,10,1,1),oma=c(0,0,0,0),cex.axis=1.5)
linewidth=3
xlims <- c(0,450)
ylims=c(1000,16000)
plottype="o"
pointtype=16

dat.d.m <- summaryBy(count~depth+Date+HWtrt,data=probe.hw,FUN=mean,keep.names=T)

#- plot  control 
plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="C"),xlim=xlims,ylim=ylims,
       legend=F,type=plottype,lwd=linewidth,pch=pointtype,
       axes=F,xlab="",ylab="")
magaxis(side=c(1,2),labels=c(0,1),frame.plot=T,las=1)
legend("bottomleft",letters[1],bty="n",cex=1.5)
legend("top","Control",bty="n",cex=1.5)
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1)
title(ylab="VWC (neutron probe counts)",outer=T,line=-3,cex.lab=2.5,xpd=NA,adj=0.6)


#- plot  heatwave
plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="HW"),xlim=xlims,ylim=ylims,
       legend=F,type=plottype,lwd=linewidth,pch=pointtype,
       axes=F,xlab="",ylab="")
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1)
legend("bottomleft",letters[2],bty="n",cex=1.5)
legend("top","Heatwave",bty="n",cex=1.5)
legend("bottomright",lwd=linewidth,legend=levels(as.factor(probe.hw$Date)),col=palette()[1:5],title="Date",cex=1.5)
title(ylab="VWC (neutron probe counts)",outer=T,line=-39,cex.lab=2.5,xpd=NA,adj=0.6)


# plot uptake during heatwave conditions
plotBy(count.diff.mean~depth|HWtrt,data=probedat.diff.m,type="o",lwd=3,lty=1,pch=16,axes=F,
      ylim=c(-200,1500),xlim=xlims,cex=1.5,
      ylab="",xlab="",col=c("blue","red"),legend=F)
adderrorbars(x=probedat.diff.m$depth,y=probedat.diff.m$count.diff.mean,SE=probedat.diff.m$count.diff.se,
             direction="updown",col=c("blue","red")[probedat.diff.m$HWtrt],barlen=0.05,lwd=0.5)
legend("top","Change during heatwave period",bty="n",cex=1.5)
title(ylab="change in VWC (neutron probe counts)",outer=T,line=-74,cex.lab=2.5,xpd=NA,adj=0.6)

magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1)
legend("bottomleft",letters[3],bty="n",cex=1.5)
legend(x=240,y=1000,lwd=3,col=c("blue","red"),legend=c("Control","Heatwave"),title="Treatment",cex=1.5)
abline(h=0,lty=2)


title(xlab="Depth in soil profile (cm)",outer=T,line=-3,cex.lab=2.5,xpd=NA,adj=0.55)

# 
# #- plot warmed control 
# plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="C" & T_treatment=="elevated"),xlim=xlims,ylim=ylims,
#        legend=F,type=plottype,lwd=linewidth,pch=pointtype,
#        axes=F,xlab="",ylab="")
# magaxis(side=c(1,2),labels=c(0,1),frame.plot=T,las=1)
# legend("bottomleft",letters[3],bty="n",cex=1.5)
# legend("top","Warmed-Control",bty="n")
# magaxis(side=c(1,2,4),labels=c(1,0,0),frame.plot=T,las=1)
# 
# 
# #- plot warmed heatwave
# plotBy(count~depth|Date,data=subset(dat.d.m,HWtrt=="HW" & T_treatment=="elevated"),xlim=xlims,ylim=ylims,
#        legend=F,type=plottype,lwd=linewidth,pch=pointtype,
#        axes=F,xlab="",ylab="")
# magaxis(side=c(1,2,4),labels=c(1,0,1),frame.plot=T,las=1)
# legend("bottomleft",letters[4],bty="n",cex=1.5)
# legend("top","Warmed-Heatwave",bty="n")



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
