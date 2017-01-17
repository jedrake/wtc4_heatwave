
# Load packages, code.
source("R/load.R")

# Map the HIE-Data2 network drive. Store drive letter here.
hiedrive <- "W:"

# Flag to email the figure or not.
email_figures <- FALSE

# Directory with height / diameter measurements
hddir <- file.path(hiedrive, "WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data")

# Make full dataset
hddata <- make_hddata(hddir)

#- calculate means for each treatment
hddata.m <- summaryBy(Stem_length_cm+Diam_65cm~Date+T_treatment,data=hddata,FUN=mean,keep.names=T)

# Calculate stem volume (for planted trees only). Won't run properly until I fix the issue with forking trees.
#vol <- return_volume(hddata)

# Make figures as pdf
to.pdf(figure_diamHeight_timeseries(hddata,type="chamber"), "output/figure_diamHeight_timeseries.pdf")
#plotVol(vol=vol,output=T)
to.pdf(figure_diamHeight_heatwave(hddata,type="chamber"), "output/figure_diamHeight_HeatWave.pdf")


# Mail to send figures.
if(email_figures){
  .body <- paste(c("WTC4 - repeated tree measurements.",
                   sprintf("Data up to %s", format(max(hddata$Date))),
                   "\nThis is an automated email. Please do not reply.",
                   "Admin: remkoduursma@gmail.com"),
                 collapse="\n")
  
  send.mail(from = "wtcdatastream@gmail.com",
            to = c("remkoduursma@gmail.com","r.duursma@westernsydney.edu.au",
                   "je.drake@westernsydney.edu.au",
                   "m.tjoelker@westernsydney.edu.au","a.varhammar@westernsydney.edu.au"),
            subject = "WTC4 : height and diameter measurements",
            body = .body,
            smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "wtcdatastream", 
                        passwd = readLines("password.txt", warn=FALSE), ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = "output/figure_diamHeight_timeseries.pdf")
}



