#  File src/library/utils/R/windows/help.start.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

help.start <-
    function (update = FALSE, gui = "irrelevant",
              browser = getOption("browser"), remote = NULL)
{
    home <- if (is.null(remote)) {
        if (tools:::httpdPort == 0L) tools::startDynamicHelp()
        if (tools:::httpdPort > 0L) {
            if (update) make.packages.html(temp = TRUE)
            paste0("http://127.0.0.1:", tools:::httpdPort)
        } else stop("help.start() requires the HTTP server to be running",
                    call. = FALSE)
    } else remote
    url <- paste0(home, "/doc/html/index.html")
    cat(gettextf("If nothing happens, you should open\n%s yourself\n",
                 sQuote(url)))
    browseURL(url, browser = browser)
    invisible()
}

browseURL <- function(url, browser = getOption("browser"), encodeIfNeeded=FALSE)
{
    if(!is.character(url) || !(length(url) == 1L) || !nzchar(url))
        stop("'url' must be a non-empty character string")
    if(identical(browser, "false")) return(invisible())
    if(is.null(browser))
        shell.exec(url)
    else if (is.function(browser))
        return(invisible(browser(if(encodeIfNeeded) URLencode(url) else url)))
    else {
        if(!is.character(browser) || length(browser) != 1L || !nzchar(browser))
        stop("'browser' must be a non-empty character string")
        cmd <- paste0('"', browser, '" ',
                     if(encodeIfNeeded) URLencode(url) else url)
        system(cmd, wait=FALSE)
    }
}
