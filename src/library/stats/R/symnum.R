#  File src/library/stats/R/symnum.R
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

symnum <- function(x, cutpoints = c(  .3,  .6,	 .8,  .9, .95),
		   symbols = if(numeric.x) c(" ", ".", ",", "+", "*", "B")
		   else c(".", "|"),
		   legend = length(symbols) >= 3,
		   na = "?", eps = 1e-5, numeric.x = is.numeric(x),
		   corr = missing(cutpoints) && numeric.x,
		   show.max = if(corr) "1", show.min = NULL,
		   abbr.colnames = has.colnames,
		   lower.triangular = corr && is.numeric(x) && is.matrix(x),
		   diag.lower.tri = corr && !is.null(show.max))
{
    ## Martin Maechler, 21 Jan 1994; Dedicated to Benjamin Schaad, born that day

    ##--------------- Argument checking -----------------------------
    if(length(x) == 0L)
	return(noquote(if(is.null(d <- dim(x)))character() else array("", dim=d)))
    has.na <- any(nax <- is.na(x))
    if(numeric.x) {
	force(corr) # missingness..
	cutpoints <- sort(cutpoints)
	if(corr) cutpoints <- c(0, cutpoints, 1)
	if(anyDuplicated(cutpoints) ||
	   (corr && (any(cutpoints > 1) || any(cutpoints < 0)) ))
	    stop(if(corr) gettext("'cutpoints' must be unique in 0 < cuts < 1, but are = ")
                 else gettext("'cutpoints' must be unique, but are = "),
                 paste(format(cutpoints), collapse="|"), domain = NA)
	nc <- length(cutpoints)
	minc <- cutpoints[1L]
	maxc <- cutpoints[nc]
	range.msg <- if(corr) gettext("'x' must be between -1 and 1")
        else gettextf("'x' must be between %s and %s",
                      format(minc), format(maxc))
	if(corr) x <- abs(x)
	else
	    if(any(x < minc - eps, na.rm=TRUE)) stop(range.msg, domain = NA)
	if (   any(x > maxc + eps, na.rm=TRUE)) stop(range.msg, domain = NA)

	ns <- length(symbols)
	symbols <- as.character(symbols)
	if(anyDuplicated(symbols))
	    stop("'symbols' must be unique, but are = ",
                 paste(symbols, collapse="|"), domain = NA)
	if(nc != ns+1)
            if(corr)
                stop("number of 'cutpoints' must be one less than number of symbols")
            else
                stop("number of 'cutpoints' must be one more than number of symbols")

	iS <- cut(x, breaks = cutpoints, include.lowest = TRUE, labels = FALSE)
	if(any(ii <- is.na(iS))) {
	    ##-- can get 0, if x[i]== minc  --- only case ?
	    iS[which(ii)[!is.na(x[ii]) & (abs(x[ii] - minc) < eps)]] <- 1#-> symbol[1L]
	}
    }
##     else if(!is.logical(x))
## 	stop("'x' must be numeric or logical")
    else  { ## assume logical x : no need for cut(points)
	if(!missing(symbols) && length(symbols) != 2L)
	    stop("must have 2 'symbols' for logical 'x' argument")
	iS <- x + 1 # F = 1,  T = 2
    }
    if(has.na) {
	ans <- character(length(iS))
	if((has.na <- is.character(na)))
	    ans[nax] <- na
	ans[!nax] <- symbols[iS[!nax]]
    } else ans <- symbols[iS]
    if(numeric.x) {
	if(!is.null(show.max)) ans[x >= maxc - eps] <-
	    if(is.character(show.max)) show.max else format(maxc, dig=1)
	if(!is.null(show.min)) ans[x <= minc + eps] <-
	    if(is.character(show.min)) show.min else format(minc, dig=1)
    }
    if(lower.triangular && is.matrix(x))
	ans[!lower.tri(x, diag = diag.lower.tri)] <- ""
    attributes(ans) <- attributes(x)
    if(is.array(ans)&& (rank <- length(dim(x))) >= 2L) { # `fix' column names
	has.colnames <- !is.null(dimnames(ans))
	if(!has.colnames) {
	    dimnames(ans) <- vector("list",rank)
	} else {
	    has.colnames <- length(dimnames(ans)[[2L]]) > 0L
	}
	if((is.logical(abbr.colnames) || is.numeric(abbr.colnames))
	   && abbr.colnames) {
	    dimnames(ans)[[2L]] <-
		abbreviate(dimnames(ans)[[2L]], minlength = abbr.colnames)
	    ## dropped further abbrev. depending on getOption("width")
	}
	else if(is.null(abbr.colnames) || is.null(dimnames(ans)[[2L]]))
	    dimnames(ans)[[2L]] <- rep("", dim(ans)[2L])
	else if(!is.logical(abbr.colnames)) stop("invalid 'abbr.colnames'")
    }
    if(legend) {
	legend <- c(rbind(sapply(cutpoints,format),
			  c(sQuote(symbols),"")),
		    if(has.na) paste("	    ## NA:", sQuote(na)))
	attr(ans,"legend") <- paste(legend[-2*(ns+1)], collapse=" ")
    }
    noquote(ans)
}
