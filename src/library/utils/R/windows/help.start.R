help.start <- function(update = TRUE, gui = "irrelevant",
                       browser = getOption("browser"))
{
    a <- file.path(R.home(), "doc/html/rwin.html")
    if(!file.exists(a))
        a <- file.path(R.home(), "doc/html/rwin.htm")
    if(!file.exists(a))
        stop("unable to find the html help")
    if(update) {
        cat("updating HTML package listing\n")
        flush.console()
        try(make.packages.html(.libPaths()))
        cat("updating HTML search index\n")
        flush.console()
        try(make.search.html(.libPaths()))
        if(any(.libPaths() != .Library)) {
            cat("fixing URLs in non-standard libraries\n")
            flush.console()
            try(fixup.libraries.URLs(.libPaths()))
        }
    }
    a <- chartr("/", "\\", a)
    cat("If nothing happens, you should open `", a, "' yourself\n")
    browseURL(a, browser = browser)
    invisible("")
}

browseURL <- function(url, browser = getOption("browser"))
{
    if(!is.character(url) || !(length(url) == 1) || (nchar(url) == 0))
        stop("url must be a non-empty character string")
    if(is.null(browser))
        shell.exec(url)
    else {
        cmd <- paste('"', browser, '" ', url, sep="")
        system(cmd, wait=FALSE)
    }
}
