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
    function (update = FALSE, gui = "irrelevant",
              browser = getOption("browser"), remote = NULL)
{
    ## should always be set, but might be empty
    if (!is.function(browser) && (length(browser) != 1 || !is.character(browser) || !nzchar(browser)))
	stop("invalid browser name, check options(\"browser\").")
    home <- if (is.null(remote)) {
        if (tools:::httpdPort == 0L) tools::startDynamicHelp()
        if (tools:::httpdPort > 0L) {
            if (update) make.packages.html()
            paste("http://127.0.0.1:", tools:::httpdPort, sep = "")
        } else stop("help.start() requires the HTTP server to be running",
                    call. = FALSE)
    } else remote
    url <- paste(home, "/doc/html/index.html", sep = "")

    if (is.character(browser)) {
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
    ## escape characters.  ' can occur in URLs, so we must use " to
    ## delimit the URL.  We need to escape $, but "`\ do not occur in
    ## valid URLs (RFC 2396, on the W3C site).
    shQuote <- function(string)
        paste('"', gsub("\\$", "\\\\$", string), '"', sep="")

    if (!is.character(url) || length(url) != 1L|| !nzchar(url))
        stop("'url' must be a non-empty character string")
    if (is.function(browser))
        return(invisible(browser(if(encodeIfNeeded) URLencode(url) else url)))
    if (!is.character(browser)
       || length(browser) != 1L
       || !nzchar(browser))
        stop("'browser' must be a non-empty character string")

    if (.Platform$GUI == "AQUA" ||
        length(grep("^(localhost|):", Sys.getenv("DISPLAY"))) )
      isLocal <- TRUE
    else
      isLocal <- FALSE

    quotedUrl <- shQuote(if(encodeIfNeeded) URLencode(url) else url)
    remoteCmd <- if (isLocal)
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

make.packages.html <-
    function(lib.loc = .libPaths(), temp = TRUE, verbose = TRUE)
{
    f.tg <- if (temp) {
        dir.create(file.path(tempdir(), ".R/doc/html"), recursive = TRUE,
                   showWarnings = FALSE)
        file.path(tempdir(), ".R/doc/html/packages.html")
    } else file.path(R.home("doc"), "html", "packages.html")
    op <- file.path(tempdir(), ".R/doc/html/libPaths.rds")
    if (temp && file.exists(f.tg) && file.exists(op)) {
        ## check if we can avoid remaking it.
        old <- .readRDS(op)$libs
        if(identical(lib.loc, old)) {
            dates <- file.info(c(f.tg, lib.loc))$mtime
            if(which.max(dates) == 1L) return(TRUE)
        }
    }
    if (!file.create(f.tg)) {
        # warning("cannot update HTML package index")
        return(FALSE)
    }
    if (verbose) {
        message("Making packages.html", " ", appendLF = FALSE)
        flush.console()
    }
    file.append(f.tg,
                file.path(R.home("doc"), "html", "packages-head-utf8.html"))
    out <- file(f.tg, open = "a")
    on.exit(close(out))
    ## find out how many
    pkgs <- vector("list", length(lib.loc))
    names(pkgs) <- lib.loc
    for (lib in lib.loc) {
        pg <- Sys.glob(file.path(lib, "*", "DESCRIPTION"))
        pkgs[[lib]] <- sort(sub(paste(lib, "([^/]*)", "DESCRIPTION$", sep="/"),
                                "\\1", pg))
    }
    tot <- sum(sapply(pkgs, length))
    incr <- ifelse(tot > 500L, 100L, 10L)
    npkgs <- 0L
    for (lib in lib.loc) {
        if (verbose && npkgs %% incr == 0L) {
            message(".", appendLF = FALSE)
            flush.console()
        }
        cat("<p><h3>Packages in ", lib,
            '</h3>\n<p><table width="100%" summary="R Package list">\n',
            sep = "", file=out)
        for (i in pkgs[[lib]]) {
            title <- packageDescription(i, lib.loc = lib, fields = "Title",
                                        encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="../../library/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
            npkgs <- npkgs + 1L
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    if (verbose) {
        message(" ", "done")
        flush.console()
    }
    if (temp) .saveRDS(list(libs=lib.loc, npkgs=npkgs), op)
    invisible(TRUE)
}
