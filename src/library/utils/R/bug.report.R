#  File src/library/utils/R/unix/bug.report.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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
#  https://www.R-project.org/Licenses/

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
                     "  to the tracker at <https://bugs.R-project.org/>.",
                     "",
                     "  We will now try to open that website in a browser"))
        flush.console()
        Sys.sleep(2)
        browseURL("https://bugs.r-project.org/bugzilla3/index.cgi")
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

    findEmail2 <- function(x) {
        x <- paste(x, collapse = " ") # could be multiple lines
        if (grepl("mailto:", x))
            sub(".*mailto:([^ ]+).*", "\\1", x)
        else if (grepl("[^<]*<([^>]+)", x))
            sub("[^<]*<([^>]+)>.*", "\\1", x)
        else if (grepl("(^|.* )[^ ]+@[[:alnum:]._]+", x)) # too generous
            sub("(^|.* )([^ ]+@[[:alnum:]._]+).*", "\\2", x)
        else NA_character_
    }

    BR <- DESC$BugReports
    if (!is.null(BR) && nzchar(BR)) {
        BR <- trimws(BR)  # some packages have e.g. leading \n
        if (grepl("^https?://", BR)) {
            writeLines(info)
            cat("\nThis package has a bug submission web page, which we will now attempt\n",
                "to open.  The information above may be useful in your report.\n",
                "If the web page does not work, you should send email to the maintainer,\n",
                DESC$Maintainer, ".\n",
                sep = "")
            flush.console()
            Sys.sleep(2)
            browseURL(BR)
            return(invisible())
        } else {
            cat("This package has a BugReports field which is not the URL of a web page:\n\n",
                "  BugReports: ", BR, "\n\n", sep = "")
            em <- findEmail2(BR)
            if (!is.na(em)) {
                cat("It appears to contain an email address, so we will try that.\n\n")
                address <- em
            } else cat("We will ignore it and email the maintainer.\n\n")
            flush.console()
            Sys.sleep(2)
       }
    }

    CT <- DESC$Contact
    if (!is.null(CT) && nzchar(CT)) {
        cat("This package has a Contact field:\n\n",
            "  Contact: ", CT, "\n\n", sep = "")
        em <- findEmail2(CT)
        if (!is.na(em)) {
            cat("That appears to contain an email address, so we will try that\n")
            address <- em
        } else cat("We cannot make sense of that, so will ignore it.\n\n");
        flush.console()
        Sys.sleep(2)
    }

    if (missing(address)) {
        findEmail <- function(x) {
            ## extract the part within the first < >: the rest may be invalid.
            x <- paste(x, collapse = " ") # could be multiple lines
            sub("[^<]*<([^>]+)>.*", "\\1", x)
        }
        address <- findEmail(DESC$Maintainer)
    }

    create.post(instructions = c("", "<<insert bug report here>>", rep.int("", 3)),
                description = "bug report",
                subject = subject, address = address,
                filename = file, info = info, ...)
}
