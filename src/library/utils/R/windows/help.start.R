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
                       browser = getOption("browser"), searchEngine = FALSE)
{
    a <- if(searchEngine) file.path(R.home("doc"), "html", "index.html")
    else file.path(R.home("doc"), "html", "search", "SearchEngine.html")
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
    if(!is.character(url) || !(length(url) == 1) || !nzchar(url))
        stop("'url' must be a non-empty character string")
    if(is.null(browser))
        shell.exec(url)
    else if (is.function(browser))
        return(invisible(browser(url)))
    else {
        if(!is.character(browser)
           || !(length(browser) == 1)
           || !nzchar(browser))
        stop("'browser' must be a non-empty character string")
        cmd <- paste('"', browser, '" ', url, sep="")
        system(cmd, wait=FALSE)
    }
}
