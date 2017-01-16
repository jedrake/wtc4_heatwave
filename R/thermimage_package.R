
library(fields)
library(Thermimage)

# You need exiftools installed and added to the path!
# http://www.sno.phy.queensu.ca/~phil/exiftool/
#I downloaded the Windows executable, unzipped to path, removed the "(-k)" ending, and it worked

#Remko's function to get temperatures from the photos
get_therm_temperature <- function(imagefile){
  
  # Extract meta-tags from thermal image file ##
  cams <- flirsettings(imagefile, exiftool="installed", camvals="")
  cams
  
  # Set variables for calculation of temperature values from raw A/D sensor data  ####
  Emissivity <- cams$Info$Emissivity      # Image Saved Emissivity - should be ~0.95 or 0.96
  ObjectEmissivity <- 0.96                # Object Emissivity - should be ~0.95 or 0.96
  dateOriginal<-cams$Dates$DateTimeOriginal
  dateModif<-   cams$Dates$FileModificationDateTime
  PlanckR1<-    cams$Info$PlanckR1                      # Planck R1 constant for camera  
  PlanckB<-     cams$Info$PlanckB                       # Planck B constant for camera  
  PlanckF<-     cams$Info$PlanckF                       # Planck F constant for camera
  PlanckO<-     cams$Info$PlanckO                       # Planck O constant for camera
  PlanckR2<-    cams$Info$PlanckR2                      # Planck R2 constant for camera
  OD<-          cams$Info$ObjectDistance                # object distance in metres
  FD<-          cams$Info$FocusDistance                 # focus distance in metres
  ReflT<-       cams$Info$ReflectedApparentTemperature  # Reflected apparent temperature
  AtmosT<-      cams$Info$AtmosphericTemperature        # Atmospheric temperature
  IRWinT<-      cams$Info$IRWindowTemperature           # IR Window Temperature
  IRWinTran<-   cams$Info$IRWindowTransmission          # IR Window transparency
  RH<-          cams$Info$RelativeHumidity              # Relative Humidity
  h<-           cams$Info$RawThermalImageHeight         # sensor height (i.e. image height)
  w<-           cams$Info$RawThermalImageWidth          # sensor width (i.e. image width)
  
  ### Import image from flir jpg to obtain binary data
  img<-readflirJPG(imagefile, exiftool="installed")
  
  # Rotate image before plotting
  imgr<-rotate270.matrix(img)
  
  # Convert binary data to temperature ####
  
  # Consider whether you should change any of the following: 
  # ObjectEmissivity, OD, RH, ReflT, AtmosT, IRWinT, IRWinTran
  
  temperature<-raw2temp(imgr,ObjectEmissivity,OD,ReflT,AtmosT,IRWinT,IRWinTran,RH,
                        PlanckR1,PlanckB,PlanckF,PlanckO,PlanckR2)
  colnames(temperature)<-NULL
  rownames(temperature)<-NULL
  
  l <- list()
  l$temperature <- temperature
  l$h <- h
  l$caminfo <- cams$Info
  l$dates <- cams$Dates
  class(l) <- "thermdata"
  
  # Plot temperature image using fields package
  return(l)
}

#Function to plot thermal data
plot.thermdata <- function(x,...){
  
  r <- require(fields)
  if(!r)stop("Install the 'fields' package first.")
  image.plot(x$temperature, asp=x$h/x$w, zlim = c(20,50),
             bty="n", useRaster=TRUE, xaxt="n", yaxt="n", col=flirpal, legend.width=0.5)
}



#Read in photos

img <- get_therm_temperature("Data/ThermalPhotos/IR_4679.jpg")
img2 <- get_therm_temperature("Data/ThermalPhotos/IR_4691.jpg")

#Distribution of tempertures - 25 to 50 C seems like a good range to display

hist(img$temperature, col="blue", xlab="Temperature", main="")
hist(img2$temperature, add=TRUE, col="red")


#Make figure
windows(7,10)
par(mfrow=c(2,1), mar=c(1,1,1,6),oma=c(0,0,2,5)) #

image(img$temperature, asp=img$h/img$w, zlim = c(25,50),
           bty="n", useRaster=TRUE, xaxt="n", yaxt="n", col=midgreypal)
image.plot(legend.only = T,zlim = c(25,50),col=midgreypal)
legend("topright", "d", bty='n', cex=2)
image(img2$temperature, asp=img2$h/img2$w, zlim = c(25,50),
           bty="n", useRaster=TRUE, xaxt="n", yaxt="n", col=midgreypal)
legend("topright", "e", bty='n', cex=2)

