download.file <- function(url, destfile, method = "internal", quiet = FALSE)
{
    method <- match.arg(method,
                        c("internal", "wget", "lynx", "cp", "socket"))

    if(method == "internal" || method == "wget")
#    if(method == "internal")
#        status <- .Internal(download(url, destfile, quiet))
#    else if(method == "wget")
        if(quiet)
            status <- system(paste("wget --quiet '", url,
                                   "' -O", destfile, sep=""))
        else
            status <- system(paste("wget '", url,
                                   "' -O", destfile, sep=""))
    else if(method == "lynx")
        status <- system(paste("lynx -dump '", url, "' >", destfile, sep=""))
    else if(method == "cp") {
        url <- sub("^file:", "", url)
        status <- system(paste("cp", url, destfile))
    }
    else if (method == "socket") {
        status <- 0
        httpclient(url, check.MIME.type=TRUE, file=destfile)
    }

    if(status > 0)
        warning("Download had nonzero exit status")

    invisible(status)
}

