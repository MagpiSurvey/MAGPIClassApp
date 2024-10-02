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

To setup dropbox token using rdrop2, run the following 2 lines first to connect to dropbox account and keep your token safe: 
tokenfile='path/and/name/of/tokenfile'
token <- drop_auth()
#Note, tokens get automatically disabled after 4 hours (new dropbox policy), to get permanency, try the fix(es) described at https://github.com/karthik/rdrop2/issues/201
saveRDS(token, tokenfile)
