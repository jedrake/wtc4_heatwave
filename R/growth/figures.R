
figure_diamHeight_timeseries <- function(hddata,type="chamber"){
  
  #- trees that were eventually planted
  trees <- subset(hddata,Date==as.Date("2016-02-03"))$Pot_No
  if(type=="chamber"){
    hddata <- subset(hddata, Pot_No %in% trees)
  }
  
  se <- function(x){
    x <- x[!is.na(x)]
    sd(x)/sqrt(length(x))
  }
  hddata$d2h <- with(hddata,(Diam_65cm/10)^2*Stem_length_cm)
  
  dfa <- summaryBy(. ~ Date + T_treatment +HWtrt, data=hddata, 
                   FUN=c(mean,se))
  
  palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red
  
  
  pdf("Output/FigureS5-Growth_4treatments.pdf",width=5)
  par(mfrow=c(2,1),mar=c(0,7,0,1),oma=c(6,0,5,0),cex.lab=1.3)
  #---
  #- plot diameter, then height
  
  #- plot diameter (65-cm) of ambient trees
  with(subset(dfa,HWtrt=="C" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")), plot(Date-1, Diam_65cm.mean, pch=16, 
                 col=palette()[c(1,3)][T_treatment],
                 ylab="Stem diameter (mm)",axes=F,xlim=c(as.Date("2016-9-1"),as.Date("2016-11-15")),
                 xlab="",
                 ylim=c(min(Diam_65cm.mean-Diam_65cm.se,na.rm=T),max(Diam_65cm.mean+Diam_65cm.se,na.rm=T)),
                 panel.first=adderrorbars(Date-1, Diam_65cm.mean, 
                                          Diam_65cm.se, direction="updown",
                                          col=palette()[c(1,3)][T_treatment],barlen=0)
  ))
  
  #- plot diameter (65-cm) of heatwave trees
  with(subset(dfa,HWtrt=="HW"& Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")), points(Date+1, Diam_65cm.mean, pch=16, 
                                    col=palette()[c(2,4)][T_treatment],
                                    ylab="Stem diameter (mm)",axes=F,
                                    ylim=c(min(Diam_65cm.mean-Diam_65cm.se,na.rm=T),max(Diam_65cm.mean+Diam_65cm.se,na.rm=T)),
                                    panel.first=adderrorbars(Date+1, Diam_65cm.mean, 
                                                             Diam_65cm.se, direction="updown",
                                                             col=palette()[c(2,4)][T_treatment],barlen=0)
  ))
  
  
  #- add shaded rectangle for heatwave
  dates <- as.Date(c("2016-10-31","2016-11-4"),format="%Y-%m-%d")
  rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=85,col="darkgrey",density=7) 
  
  axis.Date(side=1,at=seq.Date(from=as.Date("2016-9-1"),to=as.Date("2017-1-1"),by="month"),las=2,format="%m/%Y",labels=F)
  axis(2);box();axis(4)
 
  legend(x=as.Date("2016-8-15"),y=75, legend=c("Ambient-control","Ambient-Heatwave","elevated-Control","Warmed-Heatwave"),
         pch=c(16,16,16,16), col=palette()[1:4], bty='n',ncol=2,xpd=NA) 
  legend("bottomright",legend=letters[1],cex=1.4,bty="n")
  
  #-- add linear models fit to the first four dates, extrapolated to the fifth
  lm1 <- lm(Diam_65cm.mean~Date,
            data=subset(dfa,HWtrt=="C" & T_treatment=="ambient" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm1,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[1])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm1,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[1],lwd=1.2)
  lm2 <- lm(Diam_65cm.mean~Date,
            data=subset(dfa,HWtrt=="HW" & T_treatment=="ambient" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm2,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[2])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm2,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[2],lwd=1.2)
  lm3 <- lm(Diam_65cm.mean~Date,
            data=subset(dfa,HWtrt=="C" & T_treatment=="elevated" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm3,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[3])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm3,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[3],lwd=1.2)
  lm4 <- lm(Diam_65cm.mean~Date,
            data=subset(dfa,HWtrt=="HW" & T_treatment=="elevated" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm4,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        col=palette()[4],lty=2)
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm4,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[4],lwd=1.2)
  
  
  
  
  #- plot height of control trees
  with(subset(dfa,HWtrt=="C" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")), plot(Date-1, Stem_length_cm.mean, pch=16, 
                col=palette()[c(1,3)][T_treatment],
                ylab="Height (cm)",axes=F,xlim=c(as.Date("2016-9-1"),as.Date("2016-11-15")),
                ylim=c(min(Stem_length_cm.mean-Stem_length_cm.se,na.rm=T),max(Stem_length_cm.mean+Stem_length_cm.se,na.rm=T)),
                panel.first=adderrorbars(Date-1, Stem_length_cm.mean, 
                                         Stem_length_cm.se, direction="updown",
                col=palette()[c(1,3)][T_treatment],barlen=0)
  ))
  #- plot height of heatwave trees
  with(subset(dfa,HWtrt=="HW" & Date>as.Date("2016-9-1")& Date<as.Date("2016-11-15")), points(Date+1, Stem_length_cm.mean, pch=16, 
                                                              col=palette()[c(2,4)][T_treatment],
                                                               ylab="Stem diameter (mm)",axes=F,xlim=c(as.Date("2016-9-1"),as.Date("2016-12-1")),
                                                               ylim=c(min(Stem_length_cm.mean-Stem_length_cm.se,na.rm=T),max(Stem_length_cm.mean+Stem_length_cm.se,na.rm=T)),
                                                               panel.first=adderrorbars(Date+1, Stem_length_cm.mean, 
                                                                                        Stem_length_cm.se, direction="updown",
                                                                                        col=palette()[c(2,4)][T_treatment],barlen=0)
  ))
  
  
  #- add shaded rectangle for heatwave
  dates <- as.Date(c("2016-10-31","2016-11-4"),format="%Y-%m-%d")
  rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=1000,col="darkgrey",density=7) 
  
  axis.Date(side=1,at=seq.Date(from=as.Date("2016-9-1"),to=as.Date("2017-1-1"),by="month"),las=2,format="%m/%Y")
  axis(2);box();axis(4)
  legend("bottomright",legend=letters[2],cex=1.4,bty="n")
  
  
  #-- add linear models fit to the first four dates, extrapolated to the fifth
  lm5 <- lm(Stem_length_cm.mean~Date,
            data=subset(dfa,HWtrt=="C" & T_treatment=="ambient" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm5,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[1])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm5,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[1],lwd=1.2)
  lm6 <- lm(Stem_length_cm.mean~Date,
            data=subset(dfa,HWtrt=="HW" & T_treatment=="ambient" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm6,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        col=palette()[2],lty=2)
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm6,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[2],lwd=1.2)
  lm7 <- lm(Stem_length_cm.mean~Date,
           data=subset(dfa,HWtrt=="C" & T_treatment=="elevated" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm7,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[3])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm7,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[3],lwd=1.2)
  lm8 <- lm(Stem_length_cm.mean~Date,
            data=subset(dfa,HWtrt=="HW" & T_treatment=="elevated" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm8,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        col=palette()[4],lty=2)
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm8,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[4],lwd=1.2)
  dev.off() 
}






#- alternate version with only two treatments
figure_diamHeight_timeseries2trt <- function(hddata,type="chamber"){
  
  #- trees that were eventually planted
  trees <- subset(hddata,Date==as.Date("2016-02-03"))$Pot_No
  if(type=="chamber"){
    hddata <- subset(hddata, Pot_No %in% trees)
  }
  
  se <- function(x){
    x <- x[!is.na(x)]
    sd(x)/sqrt(length(x))
  }
  hddata$d2h <- with(hddata,(Diam_65cm/10)^2*Stem_length_cm)
  
  dfa <- summaryBy(. ~ Date  +HWtrt, data=hddata, 
                   FUN=c(mean,se))
  
  #palette(c("#00A2FF", "#F5690C", "#1C41D6", "#FF0A0A")) #shades of blue and red
  palette(c( "#1C41D6", "#FF0A0A"))
  
  pdf("Output/FigureS5-Growth_2treatments.pdf",width=5)
  par(mfrow=c(2,1),mar=c(0,7,0,1),oma=c(6,0,5,0),cex.lab=1.3)
  #---
  #- plot diameter, then height
  
  #- plot diameter (65-cm) of control trees
  with(subset(dfa,HWtrt=="C" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")), plot(Date-1, Diam_65cm.mean, pch=16, 
                                                                                            col=palette()[1],
                                                                                            ylab="Stem diameter (mm)",axes=F,xlim=c(as.Date("2016-9-1"),as.Date("2016-11-15")),
                                                                                            xlab="",
                                                                                            ylim=c(min(Diam_65cm.mean-Diam_65cm.se,na.rm=T),max(Diam_65cm.mean+Diam_65cm.se,na.rm=T)),
                                                                                            panel.first=adderrorbars(Date-1, Diam_65cm.mean, 
                                                                                                                     Diam_65cm.se, direction="updown",
                                                                                                                     col=palette()[1],barlen=0)
  ))
  
  #- plot diameter (65-cm) of heatwave trees
  with(subset(dfa,HWtrt=="HW"& Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")), points(Date+1, Diam_65cm.mean, pch=16, 
                                                                                              col=palette()[2],
                                                                                              ylab="Stem diameter (mm)",axes=F,
                                                                                              ylim=c(600,900),#ylim=c(min(Diam_65cm.mean-Diam_65cm.se,na.rm=T),max(Diam_65cm.mean+Diam_65cm.se,na.rm=T)),
                                                                                              panel.first=adderrorbars(Date+1, Diam_65cm.mean, 
                                                                                                                       Diam_65cm.se, direction="updown",
                                                                                                                       col=palette()[2],barlen=0)
  ))
  
  
  #- add shaded rectangle for heatwave
  dates <- as.Date(c("2016-10-31","2016-11-4"),format="%Y-%m-%d")
  rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=85,col="darkgrey",density=7) 
  
  axis.Date(side=1,at=seq.Date(from=as.Date("2016-9-1"),to=as.Date("2017-1-1"),by="month"),las=2,format="%m/%Y",labels=F)
  axis(2);box();axis(4)
  
  legend(x=as.Date("2016-9-15"),y=64, legend=c("Control","Heatwave"),
         pch=c(16,16), col=palette()[1:2], bty='n',ncol=2,xpd=NA) 
  legend("bottomright",legend=letters[1],cex=1.4,bty="n")
  
  #-- add linear models fit to the first four dates, extrapolated to the fifth
  lm1 <- lm(Diam_65cm.mean~Date,
            data=subset(dfa,HWtrt=="C" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm1,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[1])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm1,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[1],lwd=1.2)
  lm2 <- lm(Diam_65cm.mean~Date,
            data=subset(dfa,HWtrt=="HW" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm2,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[2])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm2,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[2],lwd=1.2)
  
  
  #- plot height of control trees
  with(subset(dfa,HWtrt=="C" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")), plot(Date-1, Stem_length_cm.mean, pch=16, 
                                                                                            col=palette()[1],
                                                                                            ylab="Height (cm)",axes=F,xlim=c(as.Date("2016-9-1"),as.Date("2016-11-15")),
                                                                                            ylim=c(600,900),#ylim=c(min(Stem_length_cm.mean-Stem_length_cm.se,na.rm=T),max(Stem_length_cm.mean+Stem_length_cm.se+15,na.rm=T)),
                                                                                            panel.first=adderrorbars(Date-1, Stem_length_cm.mean, 
                                                                                                                     Stem_length_cm.se, direction="updown",
                                                                                                                     col=palette()[1],barlen=0)
  ))
  #- plot height of heatwave trees
  with(subset(dfa,HWtrt=="HW" & Date>as.Date("2016-9-1")& Date<as.Date("2016-11-15")), points(Date+1, Stem_length_cm.mean, pch=16, 
                                                                                              col=palette()[2],
                                                                                              ylab="Stem diameter (mm)",axes=F,xlim=c(as.Date("2016-9-1"),as.Date("2016-12-1")),
                                                                                              ylim=c(min(Stem_length_cm.mean-Stem_length_cm.se,na.rm=T),max(Stem_length_cm.mean+Stem_length_cm.se,na.rm=T)),
                                                                                              panel.first=adderrorbars(Date+1, Stem_length_cm.mean, 
                                                                                                                       Stem_length_cm.se, direction="updown",
                                                                                                                       col=palette()[2],barlen=0)
  ))
  
  
  #- add shaded rectangle for heatwave
  dates <- as.Date(c("2016-10-31","2016-11-4"),format="%Y-%m-%d")
  rect(xleft=dates[1],ybottom=-4,xright=dates[2],ytop=1000,col="darkgrey",density=7) 
  
  axis.Date(side=1,at=seq.Date(from=as.Date("2016-9-1"),to=as.Date("2017-1-1"),by="month"),las=2,format="%m/%Y")
  axis(2);box();axis(4)
  legend("bottomright",legend=letters[2],cex=1.4,bty="n")
  
  
  #-- add linear models fit to the first four dates, extrapolated to the fifth
  lm5 <- lm(Stem_length_cm.mean~Date,
            data=subset(dfa,HWtrt=="C" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm5,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        lty=2,col=palette()[1])
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm5,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[1],lwd=1.2)
  lm6 <- lm(Stem_length_cm.mean~Date,
            data=subset(dfa,HWtrt=="HW" & Date>as.Date("2016-9-1") & Date<as.Date("2016-11-15")))
  lines(x=as.Date(c("2016-10-26","2016-11-09")),
        y=predict(lm6,newdata=data.frame(Date=as.Date(c("2016-10-26","2016-11-09")))),
        col=palette()[2],lty=2)
  lines(x=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")),
        y=predict(lm6,newdata=data.frame(Date=as.Date(c("2016-09-14","2016-09-28","2016-10-12","2016-10-26")))),
        lty=1,col=palette()[2],lwd=1.2)
  dev.off()
}





#- plot stem volume for each tree (a) and treatment average (b)
plotVol <- function(vol=vol,output=F){
  se <- function(x){
    x <- x[!is.na(x)]
    sd(x)/sqrt(length(x))
  }
  
  dfa <- summaryBy(. ~ Date + T_treatment, data=vol, 
                   FUN=c(mean,se))
  
  windows(50,60);par(mfrow=c(2,1),oma=c(2,3,5,2),mar=c(3,5,1,1),xpd="NA",cex.lab=1.5)
  
  #- plot volume for each chamber
  palette(rev(brewer.pal(12,"Paired")))
  plotBy(vol~Date|chamber,data=vol,type="o",pch=16,legend=F,
         ylab=expression(Stem~volume~(cm^3)),xlab="")
  legend(x=min(vol$Date-5),y=1.4*max(vol$vol),legend=levels(vol$chamber),fill=palette()[1:12],ncol=6)  
  
  
  #- plot treatment mean volume
  palette(c("black","red"))
  with(dfa, plot(Date, vol.mean, pch=19, col=T_treatment,las=1,
                 ylab=expression(Stem~volume~(cm^3)),
                 ylim=c(min(vol.mean-vol.se,na.rm=T),max(vol.mean+vol.se,na.rm=T)),
                 panel.first=adderrorbars(Date, vol.mean, 
                                          vol.se, direction="updown",
                                          col=T_treatment)
  ))
  #axis.Date(side=1,at=seq.Date(from=as.Date("2015-10-1"),to=max(dfa$Date),by="month"))
  legend("topleft", levels(dfa$T_treatment), pch=19, col=palette(), bty='n', title="Treatment") 
  legend("top", "Error bar is 1 SE", bty='n', cex=0.8)
  if(output==T) dev.copy2pdf(file="output/figure_stemVolume.pdf")
}




#- plot diameter and height for the FOUR treatments
#  based on proposed heatwave assignment
figure_diamHeight_heatwave <- function(hddata,type="chamber"){
  
  #- trees that were eventually planted
  trees <- subset(hddata,Date==as.Date("2016-02-03"))$Pot_No
  if(type=="chamber"){
    hddata <- subset(hddata, Pot_No %in% trees)
  }
  
  se <- function(x){
    x <- x[!is.na(x)]
    sd(x)/sqrt(length(x))
  }
  hddata$d2h <- with(hddata,(Diam_65cm/10)^2*Stem_length_cm)
  
  linkdf <- data.frame(Ch_No = levels(as.factor(hddata$Ch_No)),
                       HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW")) #swapped C08 and C012
  
  hddata2 <- merge(hddata,linkdf,by="Ch_No")
  hddata2$combotrt <- factor(paste(hddata2$T_treatment,hddata2$HWtrt,sep="_"))
  
  dfa <- summaryBy(. ~ Date + combotrt, data=hddata2, 
                   FUN=c(mean,se))
  
  palette(c("blue","black","red","orange"))
  #- plot diameter (65-cm)
  with(dfa, plot(Date, Diam_65cm.mean, pch=19, col=combotrt,
                   ylab="Stem diameter at 65cm (mm)",
                   ylim=c(min(Diam_65cm.mean-Diam_65cm.se,na.rm=T),max(Diam_65cm.mean+Diam_65cm.se,na.rm=T)),
                   panel.first=adderrorbars(Date, Diam_65cm.mean, 
                                            Diam_65cm.se, direction="updown",
                                            col=combotrt)
  ))
  axis.Date(side=1,at=seq.Date(from=as.Date("2015-10-1"),to=max(dfa$Date),by="month"))
  
  legend("topleft", levels(dfa$combotrt),
         pch=c(19,19), col=palette()[1:4], bty='n', title="Treatment") 
  legend("top", "Error bar is 1 SE", bty='n', cex=0.8)
  
  # plot height
  with(dfa, plot(Date, Stem_length_cm.mean, pch=19, col=combotrt,
                 ylab="Stem length (cm)",
                 ylim=c(min(Stem_length_cm.mean-Stem_length_cm.se,na.rm=T),max(Stem_length_cm.mean+Stem_length_cm.se,na.rm=T)),
                 panel.first=adderrorbars(Date, Stem_length_cm.mean, 
                                          Stem_length_cm.se, direction="updown",
                                          col=combotrt)
  ))
  axis.Date(side=1,at=seq.Date(from=as.Date("2015-10-1"),to=max(dfa$Date),by="month"))
  
  legend("topleft", levels(dfa$combotrt),
         pch=c(19,19), col=palette()[1:4], bty='n', title="Treatment") 
  legend("top", "Error bar is 1 SE", bty='n', cex=0.8)
  
  # plot d2h
  with(dfa, plot(Date, d2h.mean, pch=19, col=combotrt,
                 ylab="Volume index (d2h, cm^3)",
                 ylim=c(min(d2h.mean-d2h.se,na.rm=T),max(d2h.mean+d2h.se,na.rm=T)),
                 panel.first=adderrorbars(Date, d2h.mean, 
                                          d2h.se, direction="updown",
                                          col=combotrt)
  ))
  axis.Date(side=1,at=seq.Date(from=as.Date("2015-10-1"),to=max(dfa$Date),by="month"))
  
  legend("topleft", levels(dfa$combotrt),
         pch=c(19,19), col=palette()[1:4], bty='n', title="Treatment") 
  legend("top", "Error bar is 1 SE", bty='n', cex=0.8)
  
  
}