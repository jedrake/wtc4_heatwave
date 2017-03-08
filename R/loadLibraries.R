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



#' Adds error bars to a plot
#' 
#' @description Yet another function that adds error bars. The user must specify the length of the error bars.
#' @param x The x coordinates of start of the error bar
#' @param y The y coordinates of start of the error bar
#' @param SE The length of the error bar
#' @param direction One of 'up', 'down', 'right', 'left', 'updown' or 'rightleft'.
#' @param barlen The length of the cross-bar at the end of the error bar.
#' @param \ldots Additional parameters passed to \code{\link{arrows}}, such as the colour (\code{col}).
#' #' @details Simple wrapper for \code{\link{arrows}}, where \code{angle=90} and \code{code=3}. The \code{barlen} argument corresponds to \code{length} in \code{arrows}.
#' @examples
#' # A simple example. Also note that we can specify the colour of the error bars, or other parameters
#' # that arrows() recognizes.
#' x <- rnorm(20)
#' y <- x + rnorm(20)
#' se <- runif(20, 0.2,0.4)
#' plot(x,y,pch=21,bg="white",panel.first=adderrorbars(x,y,se,direction="updown", col="darkgrey"))
#' @export
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