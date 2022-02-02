#  File src/library/utils/R/readhttp.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

url.show <-
    function (url,  title = url, file = tempfile(),
              delete.file = TRUE, method, ...)
{
    if (download.file(url, destfile = file, method = method, mode = "w"))
        stop("transfer failure")
    file.show(file, delete.file = delete.file, title = title, ...)
}



defaultUserAgent <- function()
{
    Rver <- paste(R.version$major, R.version$minor, sep=".")
    Rdetails <- paste(Rver, R.version$platform, R.version$arch,
                      R.version$os)
    paste0("R (", Rdetails, ")")
}


makeUserAgent <- function(format = TRUE) {
    agent <- getOption("HTTPUserAgent")
    if (is.null(agent)) {
        return(NULL)
    }
    if (length(agent) != 1L)
        stop(gettextf("%s option must be a length one character vector or NULL",
                      sQuote("HTTPUserAgent")), domain = NA)
    if (format) paste0("User-Agent: ", agent[1L], "\r\n") else agent[1L]
}
