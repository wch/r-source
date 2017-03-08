#  File src/library/base/R/seq.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

seq <- function(...) UseMethod("seq")

seq.default <-
    function(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
             length.out = NULL, along.with = NULL, ...)
{
    if((One <- nargs() == 1L) && !missing(from)) {
	lf <- length(from)
	return(if(mode(from) == "numeric" && lf == 1L) {
	    if(!is.finite(from)) stop("'from' must be a finite number")
            1L:from
        } else if(lf) 1L:lf else integer())
    }
    if(!missing(along.with)) {
	length.out <- length(along.with)
	if(One) return(if(length.out) seq_len(length.out) else integer())
    }
    else if(!missing(length.out)) {
        len <- length(length.out)
        if(!len) stop("argument 'length.out' must be of length 1")
        if(len > 1L) {
            warning("first element used of 'length.out' argument")
            length.out <- length.out[1L]
        }
	if(!is.integer(length.out)) length.out <- ceiling(length.out)
    }
    chkDots(...)
    if (!missing(from) && length(from) != 1L) stop("'from' must be of length 1")
    if (!missing(to) && length(to) != 1L) stop("'to' must be of length 1")
    if (!missing(from) && # For seq("2","5") but not breaking seq(to=1, from=as.Date(.)):
        !is.finite(if(is.character(from)) from <- as.numeric(from) else from))
	stop("'from' must be a finite number")
    if (!missing(to) &&
        !is.finite(if(is.character(to)) to <- as.numeric(to) else to))
	stop("'to' must be a finite number")
    if(is.null(length.out))
	if(missing(by))
	    from:to
	else { # dealing with 'by'
	    del <- to - from
	    if(del == 0 && to == 0) return(to)
            if (length(by) != 1L) stop("'by' must be of length 1")
	    n <- del/by # of length 1, as {from, to, by} are
	    if(!is.finite(n)) {
		if(by == 0 && del == 0)
		    return(from)
		stop("invalid '(to - from)/by'")
	    }
	    if(n < 0L)
		stop("wrong sign in 'by' argument")
	    if(n > .Machine$integer.max)
		stop("'by' argument is much too small")

	    dd <- abs(del)/max(abs(to), abs(from))
	    if (dd < 100*.Machine$double.eps) return(from)
            if (is.integer(del) && is.integer(by)) {
                n <- as.integer(n) # truncates
                from + (0L:n) * by
            } else {
                n <- as.integer(n + 1e-10)
                x <- from + (0L:n) * by
                ## correct for possible overshot because of fuzz
                if(by > 0) pmin(x, to) else pmax(x, to)
            }
	}
    else if(!is.finite(length.out) || length.out < 0L)
	stop("'length.out' must be a non-negative number")
    else if(length.out == 0L) integer()
    else if (One) seq_len(length.out)
    else if(missing(by)) {
	# if(from == to || length.out < 2) by <- 1
	if(missing(to))
	    to <- from + length.out - 1L
	if(missing(from))
	    from <- to - length.out + 1L
	if(length.out > 2L) # not clear why these have as.vector, and not others
	    if(from == to) rep.int(from, length.out)
	    else { # *only* place we could (and did) use 'by's formal default
		by <- # integer if "easy"
		    if(is.integer(del <- to - from) & is.integer(n1 <- length.out - 1L)
		       && del %% n1 == 0L) del %/% n1 else del / n1
		as.vector(c(from, from + seq_len(length.out - 2L) * by, to))
	    }
	else as.vector(c(from, to))[seq_len(length.out)]
    }
    else if(missing(to))
	from + (0L:(length.out - 1L)) * by
    else if(missing(from))
	to - ((length.out - 1L):0L) * by
    else stop("too many arguments")
}

## In reverence to the very first versions of R which already had sequence():
sequence <- function(nvec) unlist(lapply(nvec, seq_len))
