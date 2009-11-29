bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report",
                       package = NULL, 
                       lib.loc = NULL)
{
    if (!is.null(package)) {
    	DESC <- packageDescription(package, lib.loc)
    	if (!inherits(DESC, "packageDescription")) 
    	    stop("Package '", package, "' DESCRIPTION not found.")
    	info <- paste("Package: ", DESC$Package, "\\n",
    	              " Version: ", DESC$Version, "\\n",
    	              " Maintainer: ", DESC$Maintainer, "\\n",
    	              " Built: ", DESC$Built, "\\n\\n", sep="", collapse="")
    	if (is.null(DESC$BugReports)) {
    	    if (missing(address) && !identical(DESC$Priority, "base")) 
    	    	address <- DESC$Maintainer
    	} else {
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
    } else
    	info <- character(0)
    
    create.post(instructions = "\\n<<insert bug report here>>\\n\\n\\n\\n",
                description = "bug report",
                subject = subject,
                ccaddress = ccaddress,
                method = method,
                address = address,
                file = file,
                info = info)
}
