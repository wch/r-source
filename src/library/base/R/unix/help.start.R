help.start <- function (gui = "irrelevant", browser = getOption("browser"),
			remote = NULL) {
    if(is.null(browser))
	stop("Invalid browser name, check options(\"browser\").")
    cat("Making links in ~/.R ...\n")
    .Script("sh", "help-links.sh", paste(unique(.lib.loc), collapse = " "))
    url <- paste(if (is.null(remote)) "$HOME/.R" else remote,
		 "/doc/html/index.html", sep = "")
    cat("If", browser, " is already running,\tit is *not* restarted,\n",
	"and you must switch to its window.\nOtherwise, be patient..\n")
    system(paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null || ",
		 browser, " ", url, " &", sep = ""))
    assign("help.start.has.been.run", TRUE,
           pos=match("package:base", search()))
    options(htmlhelp=TRUE)
}
