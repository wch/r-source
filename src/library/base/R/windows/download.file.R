download.file <- function(url, destfile, method,
                          quiet = FALSE, mode = "w")
{
    method <- if(missing(method)) "auto" else
    match.arg(method,
              c("auto", "internal", "wget", "lynx", "socket"))

    if(method == "auto") {
        if(capabilities("http/ftp"))
            method <- "internal"
        else if(length(grep("^file:", url)))
            method <- "internal"
        else if(system("wget --help", invisible=TRUE)==0)
            method <- "wget"
        else if(shell("lynx -help", invisible=TRUE)==0)
            method <- "lynx"
        else if (length(grep("^http:",url))==0)
            method <- "socket"
        else
            stop("No download method found")
    }
    if(method == "internal")
        status <- .Internal(download(url, destfile, quiet, mode))
    else if(method == "wget")
        if(quiet)
            status <- system(paste("wget --quiet", url, "-O", destfile))
        else
            status <- system(paste("wget", url, "-O", destfile))
    else if(method == "lynx")
        status <- shell(paste("lynx -dump", url, ">", destfile))
    else if (method == "socket") {
        status <- 0
        httpclient(url, check.MIME.type=TRUE, file=destfile)
    }

    if(status > 0)
        warning("Download had nonzero exit status")

    invisible(status)
}

