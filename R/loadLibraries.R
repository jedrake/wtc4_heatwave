#--------------------------------------------------------------
#- This script loads required R libraries and creates some
#   custom functions for downloading and plotting data.
#--------------------------------------------------------------

#- create data and output directories, if they do not exist
if(!dir.exists("Data")) dir.create(file.path("Data"),showWarnings=F)
if(!dir.exists("Output")) dir.create(file.path("Output"),showWarnings=F)



#- load libraries
Library <- function(pkg, ...){
  
  PACK <- .packages(all.available=TRUE)
  pkgc <- deparse(substitute(pkg))
  
  if(pkgc %in% PACK){
    library(pkgc, character.only=TRUE)
  } else {
    install.packages(pkgc, ...)
    library(pkgc, character.only=TRUE)
  }
  
}
Library(stringi)
Library(doBy)
Library(magicaxis)
Library(calibrate)
Library(plantecophys)
Library(oce)
Library(RColorBrewer)
Library(scales)
Library(colorRamps)
Library(RODBC)
Library(magrittr)
Library(lubridate)
Library(dplyr)
Library(car)
Library(nlme)
Library(phia)
Library(gplots)
Library(sp)

# do this once
r <- require(plotBy)
if(!r) devtools::install_bitbucket("remkoduursma/plotby")
library(plotBy)

#- load HIEv, load token
# r <- require(HIEv)
# if(!r)stop("Install the HIEv R package from bitbucket.org/remkoduursma/hiev")
# #devtools::install_bitbucket("remkoduursma/hiev")
# setToken(tokenfile="HIEv_token.txt")



#- install the geneplotter package to make density plots. This needs to be done via the method below.
## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("geneplotter")
r <- require(geneplotter)
if(!r)stop("Install the geneplotter package. See code in R/loadLibraries.R at ~line 60 for details.")





#------------------------------------------------------------------------------------------------------------------
#- function to download the zipfile containing all of the data from the library at Western Sydney University 
get_zipdata <- function(){
  
  
  #- check if the data and output directories exist. If they don't, create them.
  #dir.create(file.path("Data"),showWarnings=F)
  dir.create(file.path("Output"),showWarnings=F)
  
  #- define the file name and the URL to the data
  zipfn <- "WTC_TEMP-PARRA_HEATWAVE-FLUX-PACKAGE_L1.zip"
  url <- "http://hie-pub.westernsydney.edu.au/07fcd9ac-e132-11e7-b842-525400daae48/WTC_TEMP-PARRA_HEATWAVE-FLUX-PACKAGE_L1.zip"
  
  #- download the data, if there is no local copy
  failureFlag <- 0
  if(!file.exists(zipfn)){
    failureFlag <- try(download.file(url, zipfn, mode="wb"))
  }
  
  #- unzip the data
  unzip(zipfn, exdir=".", overwrite=TRUE)
  
  #- print an informative error message if downloading fails
  if(failureFlag !=0){
    message("Download failed. Perhaps try downloading the data manually by pointing a web-browser to (http://doi.org/10.4225/35/5a36f61f150f3")
  }
  
}
#------------------------------------------------------------------------------------------------------------------
# 
# 
# 
# #- function to download data, either from HIEv or from published repository
# download_data <- function(){
#   
#   #- get the combined temperature dataset (thermocouples, infrared, air temperature)
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_TEMPERATURES-COMBINED_20161010-20161123_L1.csv"),topath="Data")
#   
#   #- get the whole tree flux dataset.
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv"),topath="Data")
# 
#   #- get hydraulics datasets. 
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_PARRA_KLEAF-HEATWAVE_20161104_L0.csv"),topath="Data")
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_NATIVE-EMBOLISM-HEATWAVE_20161018-20161104_L0_v2.csv"),topath="Data")
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_WATERPOTENTIAL-HEATWAVE_20161019-20161107_L0.csv"),topath="Data")
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_TURGOR-LOSS-POINT-HEATWAVE_20161104_L0.csv"),topath="Data")
#   
#   #- get T50 data
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_T50-CI_20161019-20161117_L1.csv"),topath="Data")
#   
#   #- get the canopy harvest data. 
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_CANOPY-HARVEST-HEATWAVE_20161121_L0.csv"),topath="Data")
#   
#   #- get the diameter and height data
#   downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_TREE-HEIGHT-DIAMETER_20151028-20161124_L1.csv"),topath="Data")
#   
# }



#- standard error of the mean
se <- function(dat,na.rm=F,...){
  if(na.rm==T){
    dat <- subset(dat,is.na(dat)==F)
  }
  std <- sd(dat)
  n <- length(dat)
  se <- std/sqrt(n)
  return(se)
}





# Adds error bars to a plot
adderrorbars <- function(x,y,SE,direction,barlen=0.04,...){
  
  if(length(direction)>1)stop("direction must be of length one.")
  if(direction == "updown")
    direction <- c("up","down")
  else if(direction == "rightleft" | direction == "leftright")direction <- c("left","right")
  
  if("up" %in% direction)
    arrows(x0=x, x1=x, y0=y, y1=y+SE, code=3, angle=90, length=barlen,...)
  if("down" %in% direction) 
    arrows(x0=x, x1=x, y0=y, y1=y-SE, code=3, angle=90, length=barlen,...)
  if("left" %in% direction) 
    arrows(x0=x, x1=x-SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)
  if("right" %in% direction)
    arrows(x0=x, x1=x+SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)  
  
}