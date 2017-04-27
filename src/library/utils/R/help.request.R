#  File src/library/utils/R/unix/help.request.R
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

help.request <- function (subject = "", address = "r-help@R-project.org",
			  file = "R.help.request", ...)
{
    webpage <- "corresponding web page"
    catPlease <- function()
	cat("Please do this first - the",
	    webpage,"has been loaded in your web browser\n")
    go <- function(url) {
	catPlease()
	browseURL(url)
    }
    MyWrap <- function(...)
	paste(paste(strwrap(paste(...)), collapse="\n")) 

    checkPkgs <- function(pkgDescs,
			  pkgtxt = paste("packages",
                          paste(names(pkgDescs), collapse=", ")))
    {
        cat("Checking if", pkgtxt, "are up-to-date; may take some time...\n")

        stopifnot(sapply(pkgDescs, inherits, what="packageDescription"))
        fields <- .instPkgFields(NULL)
	n <- length(pkgDescs)
	iPkgs <- matrix(NA_character_, n, 2L + length(fields),
		      dimnames=list(NULL, c("Package", "LibPath", fields)))
	for(i in seq_len(n)) {
	    desc <- c(unlist(pkgDescs[[i]]),
		      "LibPath" = dirname(dirname(dirname(attr(pkgDescs[[i]],
		      "file")))))
	    nms <- intersect(names(desc), colnames(iPkgs))
	    iPkgs[i, nms] <- desc[nms]
	}

	old <- old.packages(instPkgs = iPkgs)

	if (!is.null(old)) {
	    update <- askYesNo(MyWrap("The following installed packages are out-of-date:\n",
				 paste(strwrap(rownames(old),
					       width = 0.7 *getOption("width"),
					       indent = 0.15*getOption("width")),
				       collapse="\n"),
				 "would you like to update now?"))
	    if (is.na(update)) stop("Cancelled by user")
	    if (isTRUE(update)) update.packages(oldPkgs = old, ask = FALSE)
	}
    }

    cat("Checklist:\n")
    post <- askYesNo("Have you read the posting guide?")
    if (!isTRUE(post)) return(go("https://www.r-project.org/posting-guide.html"))
    FAQ <- askYesNo("Have you checked the FAQ?")
    if (!isTRUE(FAQ)) return(go("https://cran.r-project.org/faqs.html"))
    intro <- askYesNo("Have you checked An Introduction to R?")
    if (!isTRUE(intro))
	return(go("https://cran.r-project.org/manuals.html"))
    NEWS <- askYesNo(MyWrap("Have you checked the NEWS of the latest development release?"))
    if (!isTRUE(NEWS)) return(go("https://cran.r-project.org/doc/manuals/r-devel/NEWS.html"))
    rsitesearch <- askYesNo("Have you looked on RSiteSearch?")
    if (!isTRUE(rsitesearch)) {
	catPlease()
	return(RSiteSearch(subject))
    }
    inf <- sessionInfo()
    if ("otherPkgs" %in% names(inf)) {
	oPkgs <- names(inf$otherPkgs)
        ## FIXME: inf$otherPkgs is a list of packageDescription()s
	other <-
	    askYesNo(MyWrap("You have packages",
                       paste0("(", paste(sQuote(oPkgs), collapse=", "),")"),
                       "other than the base packages loaded. ",
		       "If your query relates to one of these, have you ",
		       "checked any corresponding books/manuals and",
		       "considered contacting the package maintainer?"))
	if(!isTRUE(other)) return("Please do this first.")
    }

    page <- url("https://cran.r-project.org/bin/windows/base")
    title <- try(grep("<title>", readLines(page, 10L), fixed = TRUE, value = TRUE),
    		 silent = TRUE)
    if (!inherits(title, "try-error")) {
    	ver <- sub("^.*R-([^ ]*) for Windows.*$", "\\1", title)
    	if (getRversion() < numeric_version(ver)) {
	    update <- askYesNo(MyWrap("Your R version is out-of-date,",
				     "would you like to update now?"))
	    if (is.na(update)) stop("Cancelled by user")
	    if(isTRUE(update)) return(go(getOption("repos")))
    	}
    } else
    	warning("Unable to connect to CRAN to check R version.")
    
    if ("otherPkgs" %in% names(inf)) {
        checkPkgs(inf$otherPkgs)
    }
    
    ## A long prompt!
    code <- askYesNo(paste0("Have you written example code that is\n",
	" - minimal\n - reproducible\n - self-contained\n - commented",
	"\nusing data that is either\n",
	" - constructed by the code\n - loaded by data()\n",
	" - reproduced using dump(\"mydata\", file = \"\")\n", 
        MyWrap("and have you checked this code in a fresh R session",
		       "(invoking R with the --vanilla option if possible)",
		       "and is this code copied to the clipboard?")))
    if (!isTRUE(code))
	return(cat("\nIf your query is not directly related to code",
		   "(e.g. a general query \nabout R's capabilities),",
		   "email R-help@r-project.org directly. ",
		   "\nOtherwise prepare some example code first.\n"))
    change <- askYesNo(MyWrap("Would you like to change your subject line: ",
			     dQuote(subject), " to something more meaningful?"))
    if (is.na(change)) stop("Cancelled by user")
    if (isTRUE(change))
	subject <- readline("Enter subject: \n")

    create.post(instructions = paste(
		"\\n<<SEND AS PLAIN TEXT!>>\\n\\n",
		"\\n<<Write your query here, using your example code to illustrate>>",
		"\\n<<End with your name and affiliation>>\\n\\n\\n\\n"),
		description = "help request",
		subject = subject, address = address,
                filename = file, info = bug.report.info(), ...)
}
