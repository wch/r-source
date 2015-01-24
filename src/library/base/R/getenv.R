#  File src/library/base/R/getenv.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

Sys.getenv <- function(x = NULL, unset = "", names = NA)
{
    if (is.null(x)) {
        ## This presumes that '=' does not appear as part of the name
        ## of an environment variable.  That used to happen on Windows.
	x <- strsplit(.Internal(Sys.getenv(character(), "")), "=", fixed=TRUE)
	v <- n <- character(LEN <- length(x))
	for (i in 1L:LEN) {
	    n[i] <- x[[i]][1L]
	    v[i] <- paste(x[[i]][-1L], collapse = "=")
	}
	if (identical(names, FALSE))
	    v[sort.list(n)]
	else { # with names
	    v <- structure(v, names = n)
	    structure(class = "Dlist", # with nice print method
		      v[sort.list(n)])
	}
    } else {
        v <- .Internal(Sys.getenv(as.character(x), as.character(unset)))
	if (isTRUE(names) || (length(x) > 1L && !identical(names, FALSE)))
            structure(v, names = x)
        else v
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
