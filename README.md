MAGPIClassApp

Purpose: Shiny app for MAGPI visual classification

Author: Caroline Foster (c.foster@unsw.edu.au)

Coding language: R

Not included: mini-images, StellKinMaps and GasKinMaps files for space and proprietary reasons.

Note from the author: You are welcome to use the code herein as a basis for creating your own application. If you have found this useful, we would be grateful for an acknowledgement.

Working version of the app is deployed at https://shiny.datacentral.org.au/magpiclassapp/

Dependencies: 
shiny (available on CRAN),
shinyjs (available on CRAN),
Rfits (https://github.com/asgr/Rfits),
magicaxis (https://github.com/asgr/magicaxis),
jpeg (available on CRAN), and
rdrop2 (https://github.com/karthik/rdrop2)

Set paths to data and output directory in www/paths.R file.

To create the cmasher object file:

#############################################################################################################

#Load CMasher colours - must cite CMasher2020 - (i.e. van der Velden 2020)

#Download the CMasher-master file from github: https://github.com/1313e/CMasher and expand tar/zip file.

#If RColorBrewer is not already install, install it with:
install.packages('RColorBrewer')

#Loading RColorBrewer package.
library(RColorBrewer)

#Edit following path to point to the recently expanded colormaps folder within CMasher-master:
colormaps_path='.../CMasher-master/cmasher/colormaps'

#Looking up the names of the scales and location of the _norm.txt files:
cmr_cmaps_files=list.files(path=colormaps_path, all.files=TRUE, full.name=TRUE, recursive=FALSE,pattern='.txt')
cmr_cmaps_files=cmr_cmaps_files[2:length(cmr_cmaps_files)]

cmr_cmaps_names=list.files(path=colormaps_path, all.files=TRUE, full.name=FALSE, recursive=FALSE,pattern='.txt')
cmr_cmaps_names=cmr_cmaps_names[2:length(cmr_cmaps_names)]
cmr_cmaps_names=substr(cmr_cmaps_names, start=4, stop=(nchar(cmr_cmaps_names)-4))

#Loading all colour scales and adding them to a list object containing all scales called cmr_cmaps..
cmr_cmaps=list()
for (cmr_cmap in cmr_cmaps_names) {
  print(cmr_cmap)
  CM_RGB=read.table(cmr_cmaps_files[grep(cmr_cmaps_files,pattern=cmr_cmap)])
  colnames(CM_RGB)=c('R','G','B')
  assign(cmr_cmap, rgb(red=CM_RGB$R, green=CM_RGB$G, blue=CM_RGB$B))
  cmr_cmaps[[cmr_cmap]] = get(cmr_cmap)
}

#Create RDS file containing all CMasher colormaps into a single object:
#Edit paths to your liking.
RDSfilepath='...'
saveRDS(cmr_cmaps,file=paste0(RDSfilepath,'/cmr_cmaps.RDS'))
#Easy to load using the RDS object back into any R project using:
cmr_cmaps=readRDS(file=paste0(RDSfilepath,'/', 'cmr_cmaps.RDS'))
#######################################################################################################

To setup dropbox token using rdrop2, run the following 2 lines first to connect to dropbox account and keep your token safe: 
tokenfile='path/and/name/of/tokenfile'
token <- drop_auth()
#Note, tokens get automatically disabled after 4 hours (new dropbox policy), to get permanency, try the fix(es) described at https://github.com/karthik/rdrop2/issues/201
saveRDS(token, tokenfile)
