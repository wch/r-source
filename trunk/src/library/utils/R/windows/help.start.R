help.start <- function(update = TRUE, gui = "irrelevant",
                       browser = getOption("browser"))
{
    a <- file.path(R.home("doc"), "html", "index.html")
    if(!file.exists(a))
        a <- file.path(R.home("doc"), "html", "index.htm")
    if(!file.exists(a))
        stop("unable to find the HTML help")
    if(update) {
        cat(gettext("updating HTML package listing\n"))
        flush.console()
        try(make.packages.html(.libPaths()))
        cat("updating HTML search index\n")
        flush.console()
        try(make.search.html(.libPaths()))
        if(any(.libPaths() != .Library)) {
            cat(gettext("fixing URLs in non-standard libraries\n"))
            flush.console()
            try(fixup.libraries.URLs(.libPaths()))
        }
    }
    a <- chartr("/", "\\", a)
    cat(gettextf("If nothing happens, you should open '%s' yourself\n", a))
    browseURL(a, browser = browser)
    invisible("")
}

browseURL <- function(url, browser = getOption("browser"))
{
    if(!is.character(url) || !(length(url) == 1) || (nchar(url) == 0))
        stop("'url' must be a non-empty character string")
    if(is.null(browser))
        shell.exec(url)
    else {
        if(!is.character(browser)
           || !(length(browser) == 1)
           || (nchar(browser) == 0))
        stop("'browser' must be a non-empty character string")
        cmd <- paste('"', browser, '" ', url, sep="")
        system(cmd, wait=FALSE)
    }
}
