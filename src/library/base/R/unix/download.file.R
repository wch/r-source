download.file <- function(url, destfile, method="auto", quiet=FALSE)
{
    method <- match.arg(method,
                        c("auto", "wget", "lynx", "cp","socket"))

    if(method == "auto") {
        if(length(grep("^file:", url)))
            method <- "cp"
        else if(system("wget --help > /dev/null")==0)
            method <- "wget"
        else if(system("lynx -help > /dev/null")==0)
            method <- "lynx"
        else if (length(grep("^http:",url))==0)
            method <- "socket"
        else
            stop("No download method found")
    }

    if(method=="wget")
        if(quiet)
            status <- system(paste("wget --quiet '", url,
                                   "' -O", destfile, sep=""))
        else
            status <- system(paste("wget '", url,
                                   "' -O", destfile, sep=""))
    else if(method=="lynx")
        status <- system(paste("lynx -dump '", url, "' >", destfile, sep=""))
    else if(method=="cp") {
        url <- sub("^file:","",url)
        status <- system(paste("cp", url, destfile))
        if(status !=0)
            status <- shell(paste("copy", url, destfile))
    }
    else if (method=="socket"){
        status<-0
        httpclient(url,check.MIME.type=TRUE,file=destfile)
    }

    if(status>0)
        warning("Download had nonzero exit status")

    invisible(status)
}

