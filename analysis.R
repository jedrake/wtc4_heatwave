#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Central analysis script for the whole-tree chamber heatwave project.
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- P R E P A R E
#-----------------------------------------------------------------------------------------------------------

#- Load libraries for analysis. This will install many libraries. Some non-standard libraries many need to
#    be installed manually. If you get error messages, follow the instructions in the comments of loadLibraries.R.
source("R/loadLibraries.R")

#- Download the data. This will place csv files in "/data"
get_zipdata()
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- F I G U R E   1
#-----------------------------------------------------------------------------------------------------------

#- The photos that make up figure 1 are in /Figure1-site_photos.jpeg
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- F I G U R E   2
#-----------------------------------------------------------------------------------------------------------

#- Run this script to process and plot the whole-tree chamber flux data (Fig. 2)
#    creates a pdf in "Output" named Figure2_fluxes.pdf .
#    This also creates Figure S4
source("R/Figure2_fluxes.R")
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- F I G U R E S  3-4
#-----------------------------------------------------------------------------------------------------------

#- Run this script to process and plot the T50 and temperature data (Fig. 3)
#    creates several pdfs in "Output" named Figure3-X.pdf .
#    Also makes Figure S3 (met and leaf temperature data)
source("R/Figure3to4_T50_temperatures.R")
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- F I G U R E   5
#-----------------------------------------------------------------------------------------------------------

#- Run this script to do the model evaluation (Figure 5). This takes a few minutes.
#    This generates some error messages where the leaf energy balance had two solutions.
#    For the purpose of this work, these errors can be safely ignored.
#    To generate the supplemental plots for the Leuning and Ball-Berry models (Figs. S10-S11), uncomment and run code
#      at the end of the Figure5-model.R script.
source("R/Figure5_modeling.R")
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- S U P P L E M E N T A L     T H I N G S
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- F I G U R E S
#-----------------------------------------------------------------------------------------------------------

#- Run this script to plot the TDR measurements of soil water content (Fig. S1)
source("R/plot_VWC_TDR.R")

#- Run this script to plot the neutron probe measurements of soil water content (Fig. S2)
#source("R/plot_neutron_probe.R") # data are not included in package. Ask John for them.

#- Run this script to plot leaf water potential and branch and stem hydraulics (Figs. S6-S7)
source("R/plot_midday_LWP.R")

#- Run this script to plot diameter and height increment (Figure S9).
source("R/plot_growth_heatwave.R")
#-----------------------------------------------------------------------------------------------------------