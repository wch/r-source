help.start <- function (gui = "irrelevant", browser = getOption("browser"),
			remote = NULL) {
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
    cat("Making links in ~/.R ...\n")
    .Script("sh", "help-links.sh", paste(.libPaths(), collapse = " "))
    url <- paste(if (is.null(remote)) "$HOME/.R" else remote,
		 "/doc/html/index.html", sep = "")
    writeLines(strwrap(paste("If", browser, "is already running,",
                             "it is *not* restarted, and you must",
                             "switch to its window."),
                       exdent = 4))
    writeLines("Otherwise, be patient ...")
    system(paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null || ",
		 browser, " ", url, " &", sep = ""))
    assign("help.start.has.been.run", TRUE,
           pos = match("package:base", search()))
    options(htmlhelp = TRUE)
}
