system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

dir.create <- function(path)
    invisible(.Internal(dir.create(path)))

unlink <- function(x, recursive=FALSE)
    invisible(.Internal(unlink(x, recursive)))

# This new version is based on browseURL
# now works with any browser
# Modification date: Aug 8 2002
#

help.start <- function (gui = "irrelevant", browser =  getOption("browser")) 
{
    a <- file.path(R.home(), "doc", "html", "index.html")
    if (!file.exists(a)) 
        stop("I can't find the html help")
    else {
        cat("If nothing happens, you have to open `", a, "' yourself\n")
        if(is.null(browser) || browser != "netscape") 
         fname <- gsub(":","/",.Internal(truepath(a)))
        else
         fname <- gsub(":","/",a)
 
        ch <- strsplit(fname,"")[[1]][1]
        if( ch != '/') fname <- paste("/",fname,sep="")
        fname <- paste("file://",fname,sep="") 
        browseURL(url = fname, browser = browser)
    }
    invisible("")
}



applescript <-function (path , scriptname)
{
  rc <- .Internal(applescript(path,scriptname))
  if (rc != 0)
    warning("Error in AppleScript")
}

# Based on Apple Script
# Apparently Netscape /.x and MS IE 5.x handle different file paths

browseURL <- function(url, browser = getOption("browser"))
{
    if(!is.character(url) || !(length(url) == 1) || (nchar(url) == 0))
        stop("url must be a non-empty character string")
    scriptname <- file.path(R.home(),"script","tempscript")
    zz <- file(scriptname, "wa")
    if(is.null(browser)){
        cat("property target_URL : \"",url,"\"",
                    "\r open location target_URL\r",sep="",file=zz)
        }
    else
       cat("tell application \"",browser,"\"\r",
         "OpenURL \"",url,"\"\r end tell",sep="",file=zz)    
   close(zz) 
   applescript("","tempscript")
   unlink(scriptname)
}
