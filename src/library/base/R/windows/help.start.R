help.start <- function(update = TRUE, gui = "irrelevant",
                       browser = getOption("browser"))
{
    a <- file.path(R.home(), "doc/html/rwin.html")
    if(!file.exists(a))
        a <- file.path(R.home(), "doc/html/rwin.htm")
    if(!file.exists(a))
        stop("I can't find the html help")
    if(update) {
        cat("updating HTML package listing\n")
        flush.console()
        try(make.packages.html(.libPaths()))
        try(make.search.html(.Library))
    }
    a <- gsub("/", "\\\\", a)
    cat("If nothing happens, you have to open `", a, "' yourself\n")
    if(is.null(browser)) .Internal(help.start())
    else {
        cmd <- paste('"', browser, '" ', a, sep="")
        system(cmd, wait=FALSE)
    }
    invisible("")
}
