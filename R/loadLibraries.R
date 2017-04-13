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
#devtools::install_bitbucket("remkoduursma/plotby")
library(plotBy)

#- load HIEv, load token
r <- require(HIEv)
if(!r)stop("Install the HIEv R package from bitbucket.org/remkoduursma/hiev")
#devtools::install_bitbucket("remkoduursma/hiev")
setToken(tokenfile="HIEv_token.txt")



#- install the geneplotter package to make density plots
## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("geneplotter")
r <- require(geneplotter)
if(!r)stop("Install the geneplotter package. See code in R/loadLibraries.R for details.")



#- function to download data, either from HIEv or from published repository
download_data <- function(){
  
  #- get the combined temperature dataset
  downloadHIEv(hiev=searchHIEv("WTC-TEMP_PARRA-CM-TEMPERATURES_COMBINED-20161010-20161123_L1.csv"),topath="Data")
  
  #- get the whole tree flux dataset.
  downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_WTCFLUX-CANOPYTEMP_20161029-20161115_L0.csv"),topath="Data")

  #- get hydraulics datasets
  downloadHIEv(hiev=searchHIEv("WTC_TEMP_CM_PARRA_KLEAF-HEATWAVE_20161104_L0.csv"),topath="Data")
  downloadHIEv(hiev=searchHIEv("WTC_TEMP_CM_PARRA_NATIVE-EMBOLISM-HEATWAVE"),topath="Data")
  downloadHIEv(hiev=searchHIEv("WTC_TEMP_CM_PARRA_WATERPOTENTIAL-HEATWAVE"),topath="Data")
  
  #- get T50 data
  downloadHIEv(hiev=searchHIEv("WTC_TEMP-PARRA_CM_T50-CI_20161019-20161117_L1.csv"),topath="Data")
  
}



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