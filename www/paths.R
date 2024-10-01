#Define paths here:
#Relative path on dropbox where you want the output directory 
#where you want the output files saved and loaded from:
output_dir=''
#path to input file on dropbox:
catalogue_file=''
#relative path to the mini-images folder from the app folder:
miniimpath='mini-images'
#relative path to the stellar kinematic maps folder from the app folder:
stelkinpath='StellKinMaps'
#relative path to the gas kinematic maps folder from the app folder:
gaskinpath='GasKinMaps'
#path to and name of the token file:
tokenfile=''

#To create a token file (run following 2 lines first to connect to dropbox 
#accountand keep token safe): 
#token <- drop_auth()
#saveRDS(token, tokenfile)
