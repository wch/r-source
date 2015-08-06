#  File src/library/utils/R/windows/download.file.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

download.file <-
    function(url, destfile, method, quiet = FALSE, mode = "w",
             cacheOK = TRUE, extra = getOption("download.file.extra"))
{
    destfile # check supplied
    method <- if (missing(method))
	getOption("download.file.method", default = "auto")
    else
        match.arg(method, c("auto", "internal", "wininet", "libcurl",
                            "wget", "curl", "lynx"))

    if(missing(mode) && length(grep("\\.(gz|bz2|xz|tgz|zip|rda|RData)$", url))) mode <- "wb"
    if(method == "auto") {
	method <-
            if(capabilities("libcurl") &&
               ((grepl("^ftps:", url) ||
                (grepl("^https:", url) && !setInternet2(NA)))))
                "libcurl"
            else if(capabilities("http/ftp"))
		"internal"
	    else if(grepl("^file:", url)) {
		url <- URLdecode(url)
		"internal"
	    } else if(system("wget --help", invisible=TRUE) == 0L)
		"wget"
	    else if(system("curl --help", invisible=TRUE) == 0L)
		"curl"
	    else
		stop("no download method found")
    }

    switch(method,
	   "internal" =, "wininet" = {
	       status <- .External(C_download, url, destfile, quiet, mode, cacheOK,
				   method == "wininet")
	   },
	   "libcurl" = {
	       status <- .Internal(curlDownload(url, destfile, quiet, mode, cacheOK))
	   },
	   "wget" = {
	       if(length(url) != 1L || typeof(url) != "character")
		   stop("'url' must be a length-one character vector");
	       if(length(destfile) != 1L || typeof(destfile) != "character")
		   stop("'destfile' must be a length-one character vector");
	       if(quiet) extra <- c(extra, "--quiet")
	       if(!cacheOK) extra <- c(extra, "--cache=off")
	       status <- system(paste("wget",
				      paste(extra, collapse = " "),
				      shQuote(url),
				      "-O", shQuote(path.expand(destfile))))
	   },
	   "curl" = {
	       if(length(url) != 1L || typeof(url) != "character")
		   stop("'url' must be a length-one character vector");
	       if(length(destfile) != 1L || typeof(url) != "character")
		   stop("'destfile' must be a length-one character vector");
	       if(quiet) extra <- c(extra, "-s -S")
	       if(!cacheOK) extra <- c(extra, "-H 'Pragma: no-cache'")
	       status <- system(paste("curl",
				      paste(extra, collapse = " "),
				      shQuote(url),
				      " -o", shQuote(path.expand(destfile))))
	   },
	   "lynx" =
	       stop("method 'lynx' is defunct as from R 3.1.0", domain = NA))

    if(status > 0L)
        warning("download had nonzero exit status")

    invisible(status)
}
