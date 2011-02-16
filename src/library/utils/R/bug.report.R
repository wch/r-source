bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report",
                       package = NULL,
                       lib.loc = NULL)
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
        stop("Package '", package, "' DESCRIPTION not found.")
    info <- paste("Package: ", DESC$Package, "\\n",
                  " Version: ", DESC$Version, "\\n",
                  " Maintainer: ", DESC$Maintainer, "\\n",
                  " Built: ", DESC$Built, "\\n\\n", sep="", collapse="")
    if(identical(DESC$Priority, "base")) return(baseR())

    if (!is.null(DESC$BugReports)) {
        cat(gsub("\\\\n", "\n", c(info, bug.report.info())), sep="")
        cat("\nThis package has a bug submission web page, which we will now attempt\n",
            "to open.  The information above may be useful in your report. If the web\n",
            "page doesn't work, you should send email to the maintainer,\n",
            DESC$Maintainer, ".\n",
            sep="")
        flush.console()
        Sys.sleep(2)
        browseURL(DESC$BugReports)
        return(invisible())
    }

    if (missing(address)) address <- findEmail(DESC$Maintainer)
    create.post(instructions = "\\n<<insert bug report here>>\\n\\n\\n\\n",
                description = "bug report",
                subject = subject,
                ccaddress = ccaddress,
                method = method,
                address = address,
                file = file,
                info = info)
}
