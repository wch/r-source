#  File src/library/utils/R/RSiteSearch.R
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

RSiteSearch <- function(string,
                        restrict = c("functions", "vignettes", "views"),
			format = c("normal", "short"),
			sortby = c("score", "date:late", "date:early",
			"subject", "subject:descending",
			"from", "from:descending", "size", "size:descending"),
			matchesPerPage = 20)
{
    string <- paste0("http://search.r-project.org/cgi-bin/namazu.cgi?query=",
		     gsub(" ", "+", string))
    mpp <- paste0("max=", matchesPerPage)
    format <- paste0("result=", match.arg(format))

    restrictVALS <- c("functions", "vignettes", "views")
    restr <- match.arg(restrict, choices = restrictVALS, several.ok = TRUE)
    restr <- paste(paste0("idxname=", restr), collapse = "&")

    sortby <- match.arg(sortby)
    sortby <- paste0("sort=",
		     switch(sortby,
			    "score"=, "date:late"=, "date:early" = sortby,
			    "subject"		 = "field:subject:ascending",
			    "subject:descending" = "field:subject:descending",
			    "from"		 = "field:from:ascending",
			    "from:descending"	 = "field:from:descending",
			    "size"		 = "field:size:ascending",
			    "size:descending"	 = "field:size:descending"))

    ## we know this is a http:// URL, so encoding should be safe.
    ## it seems that firefox on Mac OS needs it for {...}
    ## OTOH, Namazu does not decode in, say, sort=date:late.
    qstring <- paste(URLencode(string, reserved = TRUE),
                     mpp, format, sortby, restr, sep = "&")
    browseURL(qstring)
    cat(gettextf("A search query has been submitted to %s",
                 "http://search.r-project.org"), "\n", sep = "")
    cat(gettext("The results page should open in your browser shortly\n"))
    invisible(qstring)
}
