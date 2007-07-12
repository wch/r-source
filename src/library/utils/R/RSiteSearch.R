RSiteSearch <- function(string, restrict = c("Rhelp02a", "functions", "docs"),
			format = c("normal", "short"),
			sortby = c("score", "date:late", "date:early",
			"subject", "subject:descending",
			"from", "from:descending", "size", "size:descending"),
			matchesPerPage = 20)
{
    paste0 <- function(...) paste(..., sep = "")
    string <- paste0("http://search.r-project.org/cgi-bin/namazu.cgi?query=",
		     gsub(" ", "+", string))
    mpp <- paste0("max=", matchesPerPage)
    format <- paste0("result=", match.arg(format))

    restrictVALS <- c("Rhelp02a", "Rhelp01", "functions", "docs", "R-devel")
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

    qstring <- paste(string, mpp, format, sortby, restr, sep = "&")
    browseURL(qstring)
    cat(gettext("A search query has been submitted to"),
	"http://search.r-project.org\n")
    cat(gettext("The results page should open in your browser shortly\n"))
    invisible(qstring)
}
