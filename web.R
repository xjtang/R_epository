# Web Tools
# By xjtang
#----------------------------------------------------------------

# libraries
library(RCurl)

#--------------------------------------

# download binary file 
binDownload <- function(url, output) {
  bin <- getBinaryURL(url)
  writeBin(bin, output)
  return(0)
}

#--------------------------------------

# source a url (by Tony Breyal originally)
sourceURL <- function(url) {
  script <- getURL(url, ssl.verifypeer=F)
  eval(parse(text = script), envir = .GlobalEnv)
  return(0)
}

#--------------------------------------
