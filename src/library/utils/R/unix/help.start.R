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

help.start <- function (gui = "irrelevant", browser = getOption("browser"),
			remote = NULL)
{
    if(is.null(browser))
	stop("invalid browser name, check options(\"browser\").")
    if (is.character(browser) && browser != getOption("browser")) {
        msg <- gettext("Changing the default browser (as specified by the 'browser' option) to the given browser so that it gets used for all future help requests.")
        writeLines(strwrap(msg, exdent = 4))
        options(browser = browser)
    }
    make.packages.html()
    tmpdir <- paste("file://", URLencode(tempdir()), "/.R", sep = "")
    url <- paste(if (is.null(remote)) tmpdir else remote,
		 "/doc/html/index.html", sep = "")
    if (is.character(browser)) {
        writeLines(strwrap(gettextf("If '%s' is already running, it is *not* restarted, and you must switch to its window.",
                                    browser),
                           exdent = 4))
        writeLines(gettext("Otherwise, be patient ..."))
    }
    browseURL(url)
    options(htmlhelp = TRUE)
}

browseURL <- function(url, browser = getOption("browser"))
{
    ## escape characters.  ' can occur in URLs, so we must use " to
    ## delimit the URL.  We need to escape $, but "`\ do not occur in
    ## valid URLs (RFC 2396, on the W3C site).
    shQuote <- function(string)
        paste('"', gsub("\\$", "\\\\$", string), '"', sep="")

    if(!is.character(url) || !(length(url) == 1) || !nzchar(url))
        stop("'url' must be a non-empty character string")
    if (is.function(browser))
        return(invisible(browser(url)))
    if(!is.character(browser)
       || !(length(browser) == 1)
       || !nzchar(browser))
        stop("'browser' must be a non-empty character string")

    if (.Platform$GUI == "AQUA" ||
        length(grep("^(localhost|):", Sys.getenv("DISPLAY"))) > 0)
      isLocal <- TRUE
    else
      isLocal <- FALSE

    quotedUrl <- shQuote(url)
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
    .Script("sh", "help-links.sh",tempdir())
    if(packages) {
        f.tg <- file.path(tempdir(), ".R/doc/html/packages.html")
        if(!file.create(f.tg)) {
            warning("cannot create HTML package index")
            return(FALSE)
        }
        searchindex <- file.path(tempdir(), ".R/doc/html/search/index.txt")
        if(!file.create(searchindex)) {
            warning("cannot create HTML search index")
            return(FALSE)
        }
        ## First we need to fathom out what encoding to use.
        ## For now we assume that if we have iconv then UTF-8 is OK.
        useUTF8 <- capabilities("iconv")
        file.append(f.tg, file.path(R.home("doc"), "html",
                                    if(useUTF8) "packages-head-utf8.html"
                                    else "packages-head.html"))
        out <- file(f.tg, open="a")
        search <- file(searchindex, open="w")
    }
    known <- character(0)
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
            link <- if(before == 0) i else paste(i, before, sep=".")
            from <- file.path(lib, i)
            to <- file.path(tempdir(), ".R", "library", link)
            file.symlink(from, to)
            if(!packages) next
            title <- packageDescription(i, lib.loc = lib, field = "Title",
                                        encoding = ifelse(useUTF8,"UTF-8",""))
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="../../library/', link,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
            contentsfile <- file.path(from, "CONTENTS")
            if(!file.exists(contentsfile)) next
            contents <- readLines(contentsfile)
            isURL <- grep("URL:", contents, fixed = TRUE, useBytes = TRUE)
            if(length(isURL) && link != i)
                contents[isURL] <-
                    gsub(paste("/library/", i, sep = ""),
                         paste("/library/", link, sep = ""),
                         contents[isURL], fixed = TRUE, useBytes = TRUE)
            writeLines(c(contents, ""), search)  # space between packages
        }
        if(packages)cat("</table>\n\n", file=out)
        known <- c(known, pg)
    }
    if(packages) {
        cat("</body></html>\n", file=out)
        close(out)
        close(search)
    }
    message("done")
    invisible(TRUE)
}
