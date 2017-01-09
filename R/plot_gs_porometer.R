
#-----------------------------------------------------------------------------------------------------------
# read in the porometer data, measured by john and angelica on 3 Nov 2016
pordat1 <- read.csv("data/Hydraulics/WTC_TEMP_CM_PARRA_GX-PORO_20161103.csv")
pordat1$poro <- rowMeans(pordat1[,c("poro1","poro2","poro3")])
pordat1$combotrt <- factor(paste(pordat1$T_treatment,pordat1$HW_treatment))

summaryBy(poro~HW_treatment,data=pordat1)
lm1 <- lm(poro~T_treatment*HW_treatment,data=pordat1)
anova(lm1)


#- plot
windows()
par(cex.axis=1.2,cex.lab=1.5,mar=c(12,8,1,1))
boxplot(poro~combotrt,data=pordat1,ylab="",las=2,ylim=c(0,500),
        names=rep("",4))

text(x=1:4+0.1,y=-50,labels=c("Ambient-Control","Ambient-Heatwave","Warmed-Control","Warmed-Heatwave"),xpd=T,srt=45,
     pos=2,cex=1.5)
title(ylab=expression(g[s]~(mmol~H[2]*O~m^-2~s^-1)),line=3.5)
#-----------------------------------------------------------------------------------------------------------