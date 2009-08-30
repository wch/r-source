#  File src/library/utils/R/windows/help.start.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

help.start <- function(update = TRUE, gui = "irrelevant",
                       browser = getOption("browser"))
{
    if(tools:::httpdPort == 0L) tools::startDynamicHelp()
    dynamic <- tools:::httpdPort > 0L
    home <- if (dynamic)
        paste("http://127.0.0.1:", tools:::httpdPort, sep = "")
    else R.home()
    a <- file.path(home, "doc", "html", "index.html")
    if(!dynamic && !file.exists(a))
        stop("unable to find the HTML help")
    if(update) {
        if(dynamic) dir.create(file.path(tempdir(), ".R/doc/html"),
                               recursive = TRUE)
        cat(gettext("updating HTML package listing\n"))
        flush.console()
        try(make.packages.html(.libPaths(),
                               outfile = if(dynamic) file.path(tempdir(), ".R/doc/html/packages.html")
                               ))
        cat("updating HTML search index\n")
        flush.console()
        try(make.search.html(.libPaths()))
    }
    cat(gettextf("If nothing happens, you should open '%s' yourself\n", a))
    if(!dynamic) a <- chartr("/", "\\", a)
    browseURL(a, browser = browser)
    invisible("")
}

browseURL <- function(url, browser = getOption("browser"), encodeIfNeeded=FALSE)
{
    if(!is.character(url) || !(length(url) == 1) || !nzchar(url))
        stop("'url' must be a non-empty character string")
    if(is.null(browser))
        shell.exec(url)
    else if (is.function(browser))
        return(invisible(browser(if(encodeIfNeeded) URLencode(url) else url)))
    else {
        if(!is.character(browser)
           || !(length(browser) == 1L)
           || !nzchar(browser))
        stop("'browser' must be a non-empty character string")
        cmd <- paste('"', browser, '" ',
                     if(encodeIfNeeded) URLencode(url) else url, sep="")
        system(cmd, wait=FALSE)
    }
}
