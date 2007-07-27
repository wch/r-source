#  File src/library/utils/R/windows/download.file.R
#  Part of the R package, http://www.R-project.org
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

download.file <- function(url, destfile, method,
                          quiet = FALSE, mode = "w", cacheOK = TRUE)
{
    method <- if (missing(method))
        ifelse(!is.null(getOption("download.file.method")),
               getOption("download.file.method"),
               "auto")
    else
        match.arg(method, c("auto", "internal", "wget", "lynx"))

    if(method == "auto") {
        if(capabilities("http/ftp"))
            method <- "internal"
        else if(length(grep("^file:", url))) {
            method <- "internal"
            url <- URLdecode(url)
        } else if(system("wget --help", invisible=TRUE)==0)
            method <- "wget"
        else if(shell("lynx -help", invisible=TRUE)==0)
            method <- "lynx"
        else
            stop("no download method found")
    }
    if(method == "internal")
        status <- .Internal(download(url, destfile, quiet, mode, cacheOK))
    else if(method == "wget") {
        extra <- if(quiet) " --quiet" else ""
        if(!cacheOK) extra <- paste(extra, "--cache=off")
        status <- system(paste("wget", extra, url, "-O",
                               path.expand(destfile)))
    } else if(method == "lynx")
        status <- shell(paste("lynx -dump", url, ">",
                              path.expand(destfile)))

    if(status > 0)
        warning("download had nonzero exit status")

    invisible(status)
}

