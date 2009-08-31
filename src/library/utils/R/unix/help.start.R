#  File src/library/utils/R/unix/help.start.R
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

help.start <-
    function (update = TRUE, gui = "irrelevant", browser = getOption("browser"),
              remote = NULL)
{
    ## should always be set, but might be empty
    if(length(browser) != 1 || !is.character(browser) || !nzchar(browser))
	stop("invalid browser name, check options(\"browser\").")
    if (is.character(browser) && browser != getOption("browser")) {
        msg <- gettext("Changing the default browser (as specified by the 'browser' option) to the given browser so that it gets used for all future help requests.")
        writeLines(strwrap(msg, exdent = 4L))
        options(browser = browser)
    }
    url <- if (is.null(remote)) {
        if(update) make.packages.html()
        if(tools:::httpdPort == 0L) tools::startDynamicHelp()
        if (tools:::httpdPort > 0L)
            paste("http://127.0.0.1:", tools:::httpdPort,
                  "/doc/html/index.html", sep = "")
        else if(file.exists(file.path(tempdir(), "/.R/doc/html/index.html")))
            paste("file://", URLencode(tempdir()),
                   "/.R/doc/html/index.html", sep = "")
        else paste("file://", R.home("doc"), "/html/index.html", sep = "")
    } else paste(remote, "/doc/html/index.html", sep = "")

    if (is.character(browser)) {
        writeLines(strwrap(gettextf("If '%s' is already running, it is *not* restarted, and you must switch to its window.",
                                    browser),
                           exdent = 4L))
        writeLines(gettext("Otherwise, be patient ..."))
    }
    browseURL(url)
    ## options(htmlhelp = TRUE)
}

browseURL <- function(url, browser = getOption("browser"), encodeIfNeeded=FALSE)
{
    ## escape characters.  ' can occur in URLs, so we must use " to
    ## delimit the URL.  We need to escape $, but "`\ do not occur in
    ## valid URLs (RFC 2396, on the W3C site).
    shQuote <- function(string)
        paste('"', gsub("\\$", "\\\\$", string), '"', sep="")

    if(!is.character(url) || length(url) != 1L|| !nzchar(url))
        stop("'url' must be a non-empty character string")
    if (is.function(browser))
        return(invisible(browser(if(encodeIfNeeded) URLencode(url) else url)))
    if(!is.character(browser)
       || length(browser) != 1L
       || !nzchar(browser))
        stop("'browser' must be a non-empty character string")

    if (.Platform$GUI == "AQUA" ||
        length(grep("^(localhost|):", Sys.getenv("DISPLAY"))) )
      isLocal <- TRUE
    else
      isLocal <- FALSE

    quotedUrl <- shQuote(if(encodeIfNeeded) URLencode(url) else url)
    remoteCmd <- if(isLocal)
        switch(basename(browser),
               "gnome-moz-remote" =, "open" = quotedUrl,
               "galeon" = paste("-x", quotedUrl),
               "kfmclient" = paste("openURL", quotedUrl),
               "mozilla" =, "opera" =, "firefox" = {
                   paste("-remote \"openURL(",
                         ## Quote ',' and ')' ...
                         gsub("([,)$])", "%\\1", url), ")\"",
                         sep = "")
               }, quotedUrl)
    else quotedUrl
    system(paste(browser, remoteCmd, "> /dev/null 2>&1 ||",
                 browser, quotedUrl, "&"))
}

make.packages.html <- function(lib.loc=.libPaths(), packages = TRUE)
{
    message("Making links in per-session dir ...", " ", appendLF = FALSE)
    ## FIXME: we could do this in R.
    ## We only need to do it whilst we support non-server HTML help
    .Script("sh", "help-links.sh", tempdir())
    if(packages) {
        f.tg <- file.path(tempdir(), ".R/doc/html/packages.html")
        if(!file.create(f.tg)) {
            warning("cannot create HTML package index")
            return(FALSE)
        }
        file.append(f.tg,
                    file.path(R.home("doc"), "html", "packages-head-utf8.html"))
        out <- file(f.tg, open="a")
    }
    known <- character(0L)
    for (lib in lib.loc) {
        if(packages)
            cat("<p><h3>Packages in ", lib,
                '</h3>\n<p><table width="100%" summary="R Package list">\n',
                sep = "", file=out)
        ## too slow
        ## pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        pg <- Sys.glob(file.path(lib, "*", "DESCRIPTION"))
        pg <- sort(sub(paste(lib, "([^/]*)", "DESCRIPTION$", sep="/"),
                       "\\1", pg))
        for (i in pg) {
            ## links are set up to break ties of package names
            before <- sum(i %in% known)
            link <- if(before == 0L) i else paste(i, before, sep=".")
            from <- file.path(lib, i)
            to <- file.path(tempdir(), ".R", "library", link)
            file.symlink(from, to)
            if(!packages) next
            title <- packageDescription(i, lib.loc = lib, fields = "Title",
                                        encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="../../library/', link,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        if(packages) cat("</table>\n\n", file=out)
        known <- c(known, pg)
    }
    if(packages) {
        cat("</body></html>\n", file=out)
        close(out)
    }
    message("done")
    invisible(TRUE)
}
