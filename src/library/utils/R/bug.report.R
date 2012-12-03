#  File src/library/utils/R/unix/bug.report.R
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

bug.report.info <- function()
    c("R Version:",
      paste0(" ", names(R.version), " = ", R.version),
      if (nzchar(Sys.getenv("R_GUI_APP_VERSION")))
          c("", "GUI:",
            paste0(" R-GUI ", Sys.getenv("R_GUI_APP_VERSION"),
                   " (", Sys.getenv("R_GUI_APP_REVISION"),")")),
      if (.Platform$OS.type == "windows") c("", win.version()),
      "",
      "Locale:", paste0(" ", Sys.getlocale()),
      "",
      "Search Path:",
      strwrap(paste(search(), collapse=", "), indent = 1, exdent = 1),
      "")

bug.report <- function(subject = "", address,
                       file = "R.bug.report", package = NULL, lib.loc = NULL,
                       ...)
{
    baseR <- function() {
        writeLines(c("  Bug reports on R and the base packages need to be submitted",
                     "  to the tracker at http://bugs.r-project.org/ .",
                     "",
                     "  We will now try to open that website in a browser"))
        flush.console()
        Sys.sleep(2)
        browseURL("https://bugs.r-project.org/bugzilla3/index.cgi")
    }

    findEmail <- function(x) {
        ## extract the part within the first < >: the rest may be invalid.
        x <- paste(x, collapse = " ") # could be multiple lines
        sub("[^<]*<([^>]+)>.*", "\\1", x)
    }
    if (is.null(package)) return(baseR())

    DESC <- packageDescription(package, lib.loc)
    if (!inherits(DESC, "packageDescription"))
        stop(gettextf("Package %s: DESCRIPTION file not found",
                      sQuote(package)), domain = NA)
    info <- paste0(c("Package", " Version", " Maintainer", " Built"),
		   ": ",
		   c(DESC$Package, DESC$Version, DESC$Maintainer, DESC$Built))
    info <- c(info, "", bug.report.info())
    if(identical(DESC$Priority, "base")) return(baseR())

    if (!is.null(DESC$BugReports)) {
        writeLines(info)
        cat("\nThis package has a bug submission web page, which we will now attempt\n",
            "to open.  The information above may be useful in your report. If the web\n",
            "page doesn't work, you should send email to the maintainer,\n",
            DESC$Maintainer, ".\n",
            sep = "")
        flush.console()
        Sys.sleep(2)
        browseURL(DESC$BugReports)
        return(invisible())
    }

    if (missing(address)) address <- findEmail(DESC$Maintainer)
    create.post(instructions = c("", "<<insert bug report here>>", rep("", 3)),
                description = "bug report",
                subject = subject, address = address,
                filename = file, info = info, ...)
}
