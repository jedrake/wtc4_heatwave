#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Plot the growth data during the heatwave
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the raw data

# Load packages, code.
source("R/growth/load.R")


# Directory with height / diameter measurements
hddir <- file.path("Data/RAW_GROWTH_DATA/WTC4 tree measure data")

# Make full dataset, merge in the heatwave trt
hddata1 <- make_hddata(hddir)
hddata1$chamber <- as.factor(paste0("C", sprintf("%02.0f", hddata1$Ch_No)))

linkdf <- data.frame(chamber = levels(as.factor(hddata1$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))

hddata <- merge(hddata1,linkdf,by="chamber")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- write out a csv to share on hiev
# hddata$HWtrt <- NULL
# hddata$Diam_15cm_1 <- NULL
# hddata$Diam_15cm_2 <- NULL
# hddata$Order_in_Ch_L_to_R <- NULL
# hddata$Ch_No <- NULL
# hddata$Plant.health <- NULL
# 
# 
# towrite <- hddata[,c(1,7,2,9,6,3:5,8,10:42)] #- this has a lot of -Inf, so get rid of those
# towrite$Stem[which(towrite$Stem < 0)] <- NA
# towrite$first_order_Brs[which(towrite$first_order_Brs < 0)] <- NA
# for (i in 9:ncol(towrite)){
#   inds <- which(towrite[,i] < 0) 
#   towrite[inds,i] <- NA
# }
# towrite$T_treatment <- as.character(towrite$T_treatment)
# towrite$T_treatment[which(towrite$T_treatment=="control")] <- "ambient"
# towrite$T_treatment[which(towrite$T_treatment=="warmed")] <- "elevated"
# 
# #- order by date, then chamber, then pot #
# towrite <- towrite[with(towrite, order(Date,chamber,Pot_No)),] 
# 
#write.csv(towrite,"Data/WTC_TEMP-PARRA_CM_TREE-HEIGHT-DIAMETER_20151028-20161124_L1.csv",row.names=F)
#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# Make figure
figure_diamHeight_timeseries(hddata,type="chamber")
figure_diamHeight_timeseries2trt(hddata,type="chamber")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
