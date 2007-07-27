#  File src/library/base/R/getenv.R
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

Sys.getenv <- function(x = NULL, unset = "")
{
    if (is.null(x)) {
        ## This presumes that '=' does not appear as part of the name
        ## of an environment variable.  That used to happen on Windows.
	x <- strsplit(.Internal(Sys.getenv(character(), "")), "=", fixed=TRUE)
	v <- n <- character(LEN <- length(x))
	for (i in 1:LEN) {
	    n[i] <- x[[i]][1]
	    v[i] <- paste(x[[i]][-1], collapse = "=")
	}
	structure(v, names = n)[sort.list(n)]
    } else {
	structure(.Internal(Sys.getenv(as.character(x), as.character(unset))),
                  names = x)
    }
}

Sys.setenv <- function(...)
{
    x <- list(...)
    nm <- names(x)
    if(is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    .Internal(Sys.setenv(nm, as.character(unlist(x))))
}

Sys.unsetenv <- function(x) .Internal(Sys.unsetenv(as.character(x)))

Sys.getpid <- function() .Internal(Sys.getpid())
