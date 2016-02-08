#  File src/library/stats/R/complete.cases.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
#  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

# complete.cases <- function(...) .External(C_compcases, ...)

complete.cases <- function(...) {
    args <- list(...)
  
    len <- -1
    check_len <- function(newlen) {
	if (len < 0) 
	    len <<- newlen
	else if (len != newlen)
	    stop("not all arguments have the same length", call. = FALSE)
    }
  
    for (s in args) {
        if (inherits(s, "data.frame"))
            check_len(nrow(s))
	else if (is.list(s)) { 
	    for (t in s) {
		check_len(NROW(t))
	    }
	} else if (!is.null(s))
	    check_len(NROW(s))
    }
    if (len < 0)
	stop("no input has determined the number of cases")

    rval <- rep(TRUE, len)
    for (s in args) {
    	if (is.list(s)) {
    	    for (t in s) {
    	    	if (is.matrix(t))
    	    	    rval[rowSums(is.na(t)) > 0] <- FALSE
    	    	else 
    	    	    rval[is.na(t)] <- FALSE
    	    }
    	} else if (is.matrix(s))
    	    rval[rowSums(is.na(s)) > 0] <- FALSE
    	else if (!is.null(s))
    	    rval[is.na(s)] <- FALSE
    }
    rval
}
 