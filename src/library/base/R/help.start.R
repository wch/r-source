help.start <- function (gui = "irrelevant", browser = "netscape", remote = NULL)
{
 url <- paste(if (is.null(remote)) "$RHOME" else remote,
              "/doc/html/index.html", sep = "")
 cat("If", browser, " is already running,\tit is *not* restarted,\n",
     "and you must switch to its window.\nOtherwise, be patient..\n")
 system(paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null || ",
              browser, " ", url, " &", sep = ""))
}
