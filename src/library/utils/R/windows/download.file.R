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
        match.arg(method, c("auto", "internal", "libcurl", "wget", "curl", "lynx"))

    if(missing(mode) & length(grep("\\.(gz|bz2|xz|tgz|zip|rda|RData)$", url))) mode <- "wb"
    if(method == "auto") {
        if(capabilities("http/ftp"))
            method <- "internal"
        else if(length(grep("^file:", url))) {
            method <- "internal"
            url <- URLdecode(url)
        } else if(system("wget --help", invisible=TRUE) == 0L)
            method <- "wget"
        else if(system("curl --help", invisible=TRUE) == 0L)
            method <- "curl"
        else
            stop("no download method found")
    }
    if(method == "internal")
        status <- .External(C_download, url, destfile, quiet, mode, cacheOK)
    else if(method == "libcurl")
        status <- .Internal(curlDownload(url, destfile, quiet, mode, cacheOK,
                                         getOption("HTTPUserAgent")))
    else if(method == "wget") {
        if(quiet) extra <- c(extra, "--quiet")
        if(!cacheOK) extra <- c(extra, "--cache=off")
        status <- system(paste("wget",
                               paste(extra, collapse = " "),
                               shQuote(url),
                               "-O", shQuote(path.expand(destfile))))
    } else if(method == "curl") {
        if(quiet) extra <- c(extra, "-s -S")
        if(!cacheOK) extra <- c(extra, "-H 'Pragma: no-cache'")
        status <- system(paste("curl",
                               paste(extra, collapse = " "),
                               shQuote(url),
                               " -o", shQuote(path.expand(destfile))))
    } else if(method == "lynx")
        stop("method 'lynx' is defunct as from R 3.1.0", domain = NA)

    if(status > 0L)
        warning("download had nonzero exit status")

    invisible(status)
}
