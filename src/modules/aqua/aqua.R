
# Setup coming from src/modules/aqua/aqua.R
# 
# sets initial current working directory to user's root
setwd("~/")
# this is to allow g77 compiler to work
Sys.putenv("PATH" = paste(Sys.getenv("PATH"),":/usr/local/bin",sep=""))


