download.file <- function(url, destfile, method = "internal",
                          quiet = FALSE, mode = "w")
{
    method <- if(missing(method)) "internal" else
    match.arg(method,
              c("internal", "wget", "lynx", "cp", "socket"))

    if(method == "internal")
        status <- .Internal(download(url, destfile, quiet, mode))
    else if(method == "wget")
        if(quiet)
            status <- system(paste("wget --quiet", url, "-O", destfile))
        else
            status <- system(paste("wget", url, "-O", destfile))
    else if(method == "lynx")
        status <- shell(paste("lynx -dump", url, ">", destfile))
    else if(method == "cp") {
        url <- sub("^file:", "", url)
        status <- system(paste("cp", url, destfile))
        if(status != 0)
            status <- shell(paste("copy", url, destfile))
    }
    else if (method == "socket") {
        status <- 0
        httpclient(url, check.MIME.type=TRUE, file=destfile)
    }

    if(status > 0)
        warning("Download had nonzero exit status")

    invisible(status)
}

