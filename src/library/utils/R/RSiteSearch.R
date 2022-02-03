#  File src/library/utils/R/RSiteSearch.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
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

RSiteSearch <- function(string,
      restrict = c("functions", "descriptions", "news", "Rfunctions",
                   "Rmanuals", "READMEs", "views", "vignettes"),
      format,
			sortby = c("score", "date:late", "date:early",
			           "subject", "subject:descending",
		             "size", "size:descending"),
			matchesPerPage = 20,
			words = c("all", "any"))
{
    matchingDB = list(functions = "cran-help", descriptions = "cran-info", news = "cran-news",
                      Rfunctions = "r-help", Rmanuals = "r-manuals", READMEs = "cran-readme",
                      views = "cran-views", vignettes = "cran-vignettes")

    ## Currently search.r-project.org uses double qoutes for phrase searching.
    ## Fomerly (before 2021) it was braces, {}.
    ## For compatibility reasons, we convert them to double qoutes, "".
    ## It also might be a convenient notation to keep.
    string <- paste0("https://search.r-project.org/?FMT=query&P=",
		     gsub("%22", '"', gsub("%20", "+",
		          URLencode(gsub("[\\{\\}]", '"', trimws(string)), reserved = TRUE),
		        fixed = TRUE), fixed = TRUE)
              )

    mpp <- paste0("HITSPERPAGE=", matchesPerPage)

    restr <- match.arg(restrict, several.ok = TRUE)
    if(length(restr) != length(restrict))
        warning("some options in argument 'restrict' were not recognized and hence ignored")
    restr <- paste(paste0("DB=", matchingDB[restr]), collapse = "&")

    sortby <- match.arg(sortby)
    sortby <- paste0("SORT=",
		     switch(sortby,
		            "score" = "",
		            "date:late" = "-0",
		            "date:early" = "%2B0",
		            "subject" = "%2B3",
		            "subject:descending" = "-3",
		            "size" = "%2B2",
		            "size:descending" = "-2")
		          )

    words <- match.arg(words)
    words <- paste0("DEFAULTOP=",
          switch(words,
                "all" = "and",
                "any" = "or")
              )

    qstring <- paste(string, mpp, sortby, restr, words, sep = "&")
    browseURL(qstring)
    
    if(!missing("format"))
        warning("argument 'format' is deprecated and has no affect on search results")
    
    cat(gettextf("A search query has been submitted to %s",
                 "https://search.r-project.org"), "\n", sep = "")
    cat(gettext("The results page should open in your browser shortly\n"))
    invisible(qstring)
}
