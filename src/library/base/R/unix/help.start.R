help.start <- function (gui = "irrelevant", browser = getOption("browser"),
			remote = NULL)
{
    if(is.null(browser))
	stop("Invalid browser name, check options(\"browser\").")
    if(browser != getOption("browser")) {
        msg <- paste("Changing the default browser",
                     "(as specified by the `browser' option)",
                     "to the given browser so that it gets used",
                     "for all future help requests.")
        writeLines(strwrap(msg, exdent = 4))
        options(browser = browser)
    }
    cat("Making links in per-session dir ...\n")
    .Script("sh", "help-links.sh",
            paste(tempdir(), paste(.libPaths(), collapse = " ")))
    tmpdir <- paste("file://", tempdir(), "/.R", sep = "")
    url <- paste(if (is.null(remote)) tmpdir else remote,
		 "/doc/html/index.html", sep = "")
    writeLines(strwrap(paste("If", browser, "is already running,",
                             "it is *not* restarted, and you must",
                             "switch to its window."),
                       exdent = 4))
    writeLines("Otherwise, be patient ...")
    browseURL(url)
    options(htmlhelp = TRUE)
}

browseURL <- function(url, browser = getOption("browser"))
{
    if(!is.character(url) || !(length(url) == 1) || (nchar(url) == 0))
        stop("url must be a non-empty character string")
    if(!is.character(browser)
       || !(length(browser) == 1)
       || (nchar(browser) == 0))
        stop("browser must be a non-empty character string")
    isLocal <- length(grep("^(localhost|):", Sys.getenv("DISPLAY"))) > 0
    remoteCmd <- if(isLocal)
        switch(basename(browser),
               "gnome-moz-remote" =, "open" = url,
               "galeon" = paste("-x", url),
               "kfmclient" = paste("openURL", url),
               "netscape" =, "mozilla" =, "opera" =, {
                   paste("-remote \"openURL(",
                         ## Quote ',' and ')' ...
                         gsub("([,)])", "%\\1", url), ")\"",
                         sep = "")
               })
    else url
    system(paste(browser, remoteCmd, "2>&1 >/dev/null ||",
                 browser, url, "&"))
}
