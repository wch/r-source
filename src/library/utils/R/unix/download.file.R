download.file <- function(url, destfile, method,
                          quiet = FALSE, mode = "w", cacheOK = TRUE)
{
    method <- if (missing(method))
        ifelse(!is.null(getOption("download.file.method")),
               getOption("download.file.method"),
               "auto")
    else
        match.arg(method, c("auto", "internal", "wget", "lynx"))

    if(method == "auto") {
        if(capabilities("http/ftp"))
            method <- "internal"
        else if(length(grep("^file:", url))) {
            method <- "internal"
            url <- URLdecode(URL)
        } else if(system("wget --help > /dev/null")==0)
            method <- "wget"
        else if(system("lynx -help > /dev/null")==0)
            method <- "lynx"
        else
            stop("no download method found")
    }
    if(method == "internal")
        status <- .Internal(download(url, destfile, quiet, mode, cacheOK))
    else if(method == "wget") {
        extra <- if(quiet) " --quiet" else ""
        if(!cacheOK) extra <- paste(extra, "--cache=off")
        status <- system(paste("wget", extra, " ", shQuote(url),
                               " -O ", path.expand(destfile), sep=""))
    } else if(method == "lynx")
        status <- system(paste("lynx -dump ", shQuote(url), " > ",
                               path.expand(destfile), sep=""))

    if(status > 0)
        warning("download had nonzero exit status")

    invisible(status)
}

nsl <- function(hostname)
    .Internal(nsl(hostname))
