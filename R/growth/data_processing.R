make_hddata <- function(hddir){
  
  #- get the file names
  fn <- dir(hddir, pattern="^WTC4.+[.]xlsx$", full.names=TRUE)
  
  #- remove the template file. Sometimes needed, sometimes not.
  if(any(grep("templete",fn))) fn <- fn[-grep("templete",fn)]
  
  #- separate the pot and in-ground files
  pot_fn <- fn[grep("Pot_Measure",fn)]
  gnd_fn <- fn[grep("Ground",fn)]
  
  #- the ground__Pot_6 file follows the same format as the pot files, so add that file to the list of pot file names
  pot_fn[length(pot_fn)+1] <- gnd_fn[grep("Ground__Pot_6",gnd_fn)]
  gnd_fn <- gnd_fn[-grep("Ground__Pot_6",gnd_fn)]
  
  #----------------------------------------------
  #- read in the pot data
  hddata_pot <- lapply(pot_fn, read_excel, skip=2)
  
  dates <- as.Date(str_extract(basename(pot_fn), "[0-9]{8}"), format="%Y%m%d")
  for(i in 1:length(hddata_pot)){
    hddata_pot[[i]]$Date <- dates[i]
    hddata_pot[[i]] <- hddata_pot[[i]][,c(1:8,ncol(hddata_pot[[i]]))] # just get the first 8 columns, plus the date
  }
  
  hddata_pot <- as.data.frame(bind_rows(hddata_pot))
  names(hddata_pot) <- make.names(names(hddata_pot))
  hddata_pot$T_treatment <- as.factor(ifelse(hddata_pot$Ch_No %% 2 == 0, "warmed", "control"))
  
  names(hddata_pot)[4:7] <- c("Diam_15cm_1","Diam_15cm_2","Stem_length_cm","first_order_Brs")
  
  
  hddata_pot$Diam_15cm <- apply(hddata_pot[,c("Diam_15cm_1","Diam_15cm_2")],1,mean,na.rm=TRUE)
  #----------------------------------------------
  
  
  
  
  #----------------------------------------------
  #- read in the ground based data
  hddata_ground <- lapply(rev(gnd_fn), read_excel, skip=2) # read last set of data as the first element. Gets the column names right later.
  
  dates <- as.Date(str_extract(basename(rev(gnd_fn)), "[0-9]{8}"), format="%Y%m%d")
  heights <- c()
  for(i in 1:length(hddata_ground)){
    hddata_ground[[i]]$Date <- dates[i]
    heights[i] <- suppressWarnings(max(as.numeric(names(hddata_ground[[i]])),na.rm=T)) # get the maximum height of measurements.
                                                                                       # eventually will be used when trees get taller.
  }
  

  hddata_ground <- as.data.frame(bind_rows(hddata_ground))
  names(hddata_ground) <- make.names(names(hddata_ground))
  hddata_ground$T_treatment <- as.factor(ifelse(hddata_ground$Ch_No %% 2 == 0, "warmed", "control"))
  hddata_ground$X <- NULL
  names(hddata_ground) <- c("Ch_No","Pot_No","Stem","Diam_15cm","Diam_30cm","Diam_65cm","Diam_100cm","Diam_130cm","Diam_160cm",
                            "Diam_190cm","Diam_220cm","Diam_250cm",
                            "Diam_280cm","Diam_310cm","Diam_340cm","Diam_370cm","Diam_400cm","Diam_430cm",
                            "Diam_460cm","Diam_490cm","Diam_520","Diam_550","Diam_580","Diam_610","Diam_640","Diam_670","Diam_700",
                            "Diam_730","Diam_760","Diam_790","Diam_820","Diam_850","Diam_880","Diam_910",
                            "Diam_940","Diam_970","Diam_1000",
                            "Stem_length_cm","Fork.diam","Date","first_order_Brs", "Insect.damage","Plant.health","comments",
                            "Order_in_Ch_L_to_R", "T_treatment")
  
  #- drop a few unwanted vars
  hddata_ground <- hddata_ground[,!(names(hddata_ground) %in% c("Insect.damage","plant.health","comments"))]
  #----------------------------------------------
  
  
  
  #----------------------------------------------
  #- Tree 3 and 12 forked as of 22 June 2016. This screws things up in this script. 
  #  Temporarily, I avoid this issue by taking the maximum of each measurement. This returns 
  #    the proper height and basal diameter, but will get volume WRONG.
  hddata_ground_noforks <- summaryBy(.~Ch_No+Pot_No+Date+T_treatment,FUN=max,data=hddata_ground,keep.names=T,na.rm=T)
  #----------------------------------------------

  
  #----------------------------------------------
  #- merge dataframes, filling missing columns with NA's. Uses plyr::rbind.fill
  hddata_all <- rbind.fill(hddata_pot,hddata_ground_noforks)
  
  #----------------------------------------------
  
  return(hddata_all)
}





#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#- get tree volume
return_volume <- function(hddata=hddata){
  #- subset to trees that were eventually planted, after they were planted
  trees <- subset(hddata,Date==as.Date("2016-02-03"))$Pot_No
  hddata2 <- subset(hddata, Pot_No %in% trees & Date > as.Date("2016-1-1"))
  
  #melt into "long" format
  hddata2$chamber <- as.factor(paste0("C",sprintf("%02.0f",hddata2$Ch_No)))
  sLong <- reshape2::melt(hddata2,measure.vars=names(hddata2)[c(11:16,18:(ncol(hddata2)-1))],
                          id.vars=c("chamber","T_treatment","Date","Stem_length_cm"),value.name="diam",variable.name="height")
  
  #- pull out the height of each measurement as a number
  numbers <- str_locate(sLong$height,"[0-9]+")
  sLong$height_n <- as.numeric(substr(sLong$height,start=numbers[,1],stop=numbers[,2]))
  
  
  # split long dataframe into a list for each chamber and each measurement date
  sLong$chamber_date <- paste(sLong$chamber,sLong$Date,sep="_")
  sLong.l <- split(sLong,sLong$chamber_date)
  
  #-- add a number for the ground (assume no taper) and a value for the maximum tree height (assume diameter of 0.1mm)
  for (i in 1:length(sLong.l)){
    # add a line to the dataframe for the diameter at floor height
    firstline <- sLong.l[[i]][1,]  
    firstline$height_n[1] <- 0 #. Edited to give Mike an estimate of total tree volume to the ground.
    
    # add a line to the dataframe for the tiny diamter at total plant height
    lastline <- sLong.l[[i]][nrow(sLong.l[[i]]),]
    lastline$height_n[1] <- lastline$Stem_length_cm[1]
    lastline$diam[1] <- 0.1 
    sLong.l[[i]] <- rbind(firstline,sLong.l[[i]],lastline)
    sLong.l[[i]] <- sLong.l[[i]][which(is.na(sLong.l[[i]]$diam)==F),] # get rid of NA's
  }
  treevol(sLong.l[[10]])
  vols <- do.call(rbind,lapply(sLong.l,FUN=treevol))
  return(vols)
}

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------






#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
# estimate stem volume from a series of measurements of diameter, and the height of that diameter measurement
# accepts a dataframe, returns a vector.

treevol <- function(dat){
  dat2 <- subset(dat,diam>0)
  
  #loop over each stem segment and calculate volume as the fustrum of a cone. Assume the (unmeasured) bottom bit is a cylinder.
  vol <- 0
  vol.cum <- 0
  for (i in 1:nrow(dat2)){
    # get the vertical segment length
    if(i==1){h1 <- 0}
    if(i > 1){h1 <- dat2[i-1,"height_n"]}
    h2 <- dat2[i,"height_n"]
    h <- h2-h1
    
    # get the radii in cm
    if(i==1){r1 <- dat2[i,"diam"]/20}
    if(i > 1){r1 <- dat2[i-1,"diam"]/20}
    r2 <- dat2[i,"diam"]/20
    
    #outer volume
    vol[i] <- pi*h/3* (r1^2+r1*r2+r2^2) # volume in cm3, fustrum of a cone. See http://jwilson.coe.uga.edu/emt725/Frustum/Frustum.cone.html
    
    
    
  }
  
  
  df.out <- dat2[1,1:4]
  df.out$diam <- max(dat2$diam)
  df.out$height <- max(dat2$Stem_length_cm)
  df.out$vol <- sum(vol,na.rm=T)
  df.out$Stem_length_cm <- NULL
  return(df.out)
}
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------






check_if_newdata <- function(df){
  
  # Last date of currently processed data
  maxdate <- max(df$Date)
  
  cfn <- "cache/maxdate.txt"
  if(!file.exists(cfn)){
    writeLines(as.character(maxdate), cfn)
    newdata <- TRUE  # not really, but must return something!
  } else {
    olddate <- as.Date(readLines(cfn))
    if(maxdate > olddate){
      newdata <- TRUE
      writeLines(as.character(maxdate), cfn)
    } else {
      newdata <- FALSE
    }
  }
  
  return(newdata)
}

tidy_hddata_forupload <- function(df){
  
  names(df)[1] <- "chamber"
  df$chamber <- as.factor(paste0("C", sprintf("%02.0f", df$chamber)))
  
  names(df)[names(df) == "Stem_lenght_cm"] <- "Stem_length"
  
  v <- c("chamber","Date","T_treatment")
  v2 <- setdiff(names(df),v)
  df <- df[,c(v,v2)]
  
  v <- 1:match("Diam_15cm_2", names(df))
  ii <- match("Diam_15cm", names(df))
  df <- df[,c(v,ii,setdiff(1:ncol(df), c(v,ii)))]
  return(df)
}


make_and_write_TOA5 <- function(data, fn, datevar="Date", datetimevar=NULL){
  
  header <- paste(c("\"TOA5\"","\"WTC\"","\"none\"","\"none\"",
                    "\"none\"","\"none\"",
                    "\"none\"","\"wtchddata\""), collapse=",")
  units <- paste(c("\"\"",
                   "\"\"",
                   "\"YYYY-MM-DD\"",
                   "\"\"",
                   "\"\"",
                   "\"\"",
                   "\"mm\"",
                   "\"mm\"",
                   "\"mm\"",
                   "\"cm\"",
                   "\"\"",
                   "\"\"",
                   "\"\"",
                   "\"\"",
                   "\"\"",
                   "\"\""),
                 collapse=",")
  
  nm <-  paste(paste("\"", c("TIMESTAMP",names(data)), "\"", sep=""), collapse=",")
  empty <- paste(paste("\"", rep("", ncol(data)), "\"", sep=""), collapse=",")
  writeLines(c(header, nm, units, empty ),   fn)
  
  if(is.null(datetimevar)){
    if(!datevar %in% names(data))stop("Must have a Date variable, set datevar argument.")
    # data <- data[order(data[,datevar]),]
    DATE <- data[,datevar]
    data$TIMESTAMP <- ISOdatetime(year(DATE), month(DATE), day(DATE),12,0,0,tz="UTC")
  } else {
    # data <- data[order(data[,datetimevar]),]
    data$TIMESTAMP <- data[,datetimevar]
  }
  data <- data[,c(ncol(data), 1:(ncol(data)-1))]
  
  names(data)[1] <- "TIMESTAMP"
  
  write.table(data, fn, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",", na="NAN")
}


# Upload data.
upload_hd_to_hiev <- function(df){
  
  # Write to this file, then upload to HIEv.
  fn <- "cache/WTC_TEMP-PARRA_CM_TREE-HEIGHT-DIAMETER_OPEN_L1.dat"
  
  # Convert to a TOA5 format (to allow auto-appending)
  make_and_write_TOA5(df, fn)
  
  # Upload.
  HIEv:::uploadToHIEv(fn, experiment=84, description=readLines("docs/hddata_metadata.txt"))
  
}






#-- make a csv with height and diameter data, for uploading to HIEv.
#  Accepts a dataframe with the processed data from the intensive surveys
makeData_height_diam <- function(hddata,hddir){
  
  #- subset the main dataframe to just have height and diameter
  hddata$chamber <-  as.factor(paste0("C", sprintf("%02.0f", hddata$Ch_No)))
  hddata2 <- hddata[,c("chamber","T_treatment","Date","Diam_65cm","Stem_length_cm")]
  
  #- read in all of the measurements with just diameter at 65cm
  fn <- dir(paste(hddir,"65cm Dia data",sep="/"), pattern="^WTC4.+[.]xlsx$", full.names=TRUE)
  diams <- list()
  for (i in 1:length(fn)){
    diams[[i]] <- read_excel(fn[i],skip=2)
    diams[[i]] <- diams[[i]][,c(1,3)]
    names(diams[[i]]) <- c("Ch_No","Diam_65cm")
    
    diams[[i]]$Date <- as.Date(substr(fn[i],start=99,stop=106),format="%Y%m%d")  
  }
  diams2 <- as.data.frame(do.call(rbind,diams))
  diams2$chamber <-  as.factor(paste0("C", sprintf("%02.0f", diams2$Ch_No)))
  diams2$T_treatment <- ifelse(diams2$Ch_No %% 2 == 1, "control","warmed")
  diams2$Stem_length_cm <- NA
  diams3 <- diams2[,c("chamber","T_treatment","Date","Diam_65cm","Stem_length_cm")]
  
  #- manually create a new dataframe for the harvest data
  diams_harvest <- data.frame(chamber=levels(diams3$chamber),
                              T_treatment=rep(c("control","warmed"),6),
                              Date=rep(as.Date("2016-11-23"),12),
                              Diam_65cm= c(63.2,54.4,54.2,68.2,53.3,69.0,50.5,67.0,53.5,50.8,41.2,69.5),
                              Stem_length_cm=c(771,915,702,974,712,1004,805,926,852,931,681,941))
  
  
  #- merge datasets
  hddata3 <- rbind(hddata2,diams3,diams_harvest)
  hddata3 <- hddata3[with(hddata3, order(chamber, Date)), ]
  
  #- change T_treatment levels to "ambient" vs. "elevated"
  hddata3$T_treatment <- as.character(hddata3$T_treatment)
  hddata3[which(hddata3$T_treatment=="control"),"T_treatment"] <- "ambient"
  hddata3[which(hddata3$T_treatment=="warmed"),"T_treatment"] <- "elevated"
  hddata3$T_treatment <- as.factor(hddata3$T_treatment)
  
  #- make a linked dataframe for the heatwave treatment assignment
  linkdf <- data.frame(chamber=levels(hddata3$chamber),	
                       HW_treatment=c("control","control","heatwave","heatwave","control","control",
                                      "heatwave","control","heatwave","heatwave","control","heatwave"))
 
  hddata4 <- merge(hddata3,linkdf,by="chamber")[,c("chamber","T_treatment","HW_treatment","Date","Diam_65cm","Stem_length_cm")]
  names(hddata4)[5:6] <- c("diam","height")
  
  #- get the maximum value observed on each date, to get rid of the many repeated measurements early on
  hddata5 <- summaryBy(diam+height~chamber+T_treatment+HW_treatment+Date,data=hddata4,FUN=max,na.rm=T,keep.names=T)
  hddata5$diam[which(hddata5$diam < 0)] <- NA # get rid of -Inf's
  hddata5$height[which(hddata5$height < 0)] <- NA
  
  #- write out a csv for sharing.
  write.csv(hddata5,row.names=F,file="WTC_TEMP_CM_PARRA_TREE-HEIGHT-DIAMETER_20151028-20161123_L0.csv")
}


