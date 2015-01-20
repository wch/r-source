#  File src/library/utils/R/help.start.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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
    WINDOWS <- .Platform$OS.type == "windows"
    if (!WINDOWS) {
        ## should always be set, but might be empty
        if (!is.function(browser) &&
            (length(browser) != 1L || !is.character(browser) || !nzchar(browser)))
            stop("invalid browser name, check options(\"browser\").")
    }
    home <- if (is.null(remote)) {
        port <- tools::startDynamicHelp(NA)
        if (port > 0L) {
            if (update) make.packages.html(temp = TRUE)
            paste0("http://127.0.0.1:", port)
        } else stop("help.start() requires the HTTP server to be running",
                    call. = FALSE)
    } else remote
    url <- paste0(home, "/doc/html/index.html")

    ## FIXME: maybe these should use message()?
    if (WINDOWS) {
        cat(gettextf("If nothing happens, you should open\n%s yourself\n", sQuote(url)))
    } else if (is.character(browser)) {
        writeLines(strwrap(gettextf("If the browser launched by '%s' is already running, it is *not* restarted, and you must switch to its window.",
                                    browser),
                           exdent = 4L))
        writeLines(gettext("Otherwise, be patient ..."))
    }
    browseURL(url, browser = browser)
    invisible()
}

browseURL <- function(url, browser = getOption("browser"), encodeIfNeeded=FALSE)
{
    WINDOWS <- .Platform$OS.type == "windows"

    if (!is.character(url) || length(url) != 1L|| !nzchar(url))
        stop("'url' must be a non-empty character string")
    if(identical(browser, "false")) return(invisible())
    if(WINDOWS && is.null(browser)) return(shell.exec(url))
    if (is.function(browser))
        return(invisible(browser(if(encodeIfNeeded) URLencode(url) else url)))

    if (!is.character(browser) || length(browser) != 1L || !nzchar(browser))
        stop("'browser' must be a non-empty character string")
    if (WINDOWS) {
        ## No shell used, but spaces are possible
        return(system(paste0('"', browser, '" ',
                             if(encodeIfNeeded) URLencode(url) else url),
                      wait = FALSE))
    }

    ## Unix-alike, character "browser"
    if (.Platform$GUI == "AQUA" ||
        length(grep("^(localhost|):", Sys.getenv("DISPLAY"))) )
      isLocal <- TRUE
    else
      isLocal <- FALSE

    ## escape characters.  ' can occur in URLs, so we must use " to
    ## delimit the URL.  We need to escape $, but "`\ do not occur in
    ## valid URLs (RFC 2396, on the W3C site).
    .shQuote <- function(string)
        paste0('"', gsub("\\$", "\\\\$", string), '"')
    quotedUrl <- .shQuote(if(encodeIfNeeded) URLencode(url) else url)
    remoteCmd <- if (isLocal)
        switch(basename(browser),
               "gnome-moz-remote" =, "open" = quotedUrl,
               "galeon" = paste("-x", quotedUrl),
               "kfmclient" = paste("openURL", quotedUrl),
               "mozilla" =, "opera" =, "firefox" = {
                   paste0("-remote \"openURL(",
                         ## Quote ',' and ')' ...
                         gsub("([,)$])", "%\\1", url), ")\"")
               }, quotedUrl)
    else quotedUrl
    system(paste(browser, remoteCmd, "> /dev/null 2>&1 ||",
                 browser, quotedUrl, "&"))
}
