#  File src/library/base/R/seq.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
    is.logint <- function(.) (is.integer(.) || is.logical(.)) && !is.object(.)
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
	intn1 <- is.integer(length.out)
    }
    else if(!missing(length.out)) {
        len <- length(length.out)
        if(!len) stop("argument 'length.out' must be of length 1")
        if(len > 1L) {
            warning("first element used of 'length.out' argument")
            length.out <- length.out[1L]
        }
	if(!(intn1 <- is.logint(length.out)))
	    length.out <- as.numeric(ceiling(length.out))
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
	    int <- is.logint(from) && is.logint(to)
	    del <- to - if(int) as.double(from) else from
	    if(del == 0 && to == 0) return(to)
            if (length(by) != 1L) stop("'by' must be of length 1")
	    if(!is.logint(by))
		int <- FALSE
	    else if(!int)
		storage.mode(by) <- "double"
	    n <- del/by # of length 1, as {from, to, by} are
	    if(!is.finite(n)) {
		if(!is.na(by) && by == 0 && del == 0)
		    return(from)
		stop("invalid '(to - from)/by'")
	    }
	    if(n < 0L)
		stop("wrong sign in 'by' argument")
	    if(n > .Machine$integer.max)
		stop("'by' argument is much too small")

	    dd <- abs(del)/max(abs(to), abs(from))
	    if (dd < 100*.Machine$double.eps) return(from)
            if (int) {
                n <- as.integer(n) # truncates
                if (n >= 2L) cumsum(rep.int(c(from, by), c(1L, n))) else
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
	if(missing(to)) {
	    to <- from + (length.out - 1)
	    intdel <- intn1 && is.logint(from) && to <= .Machine$integer.max
	    if(intdel) storage.mode(to) <- "integer"
	} else intdel <- is.logint(to)
	if(missing(from)) {
	    from <- to - (length.out - 1)
	    if(intdel) {
		intdel <- intn1 && from >= -.Machine$integer.max
		if(intdel) storage.mode(from) <- "integer"
	    }
	} else if(intdel) intdel <- is.logint(from)
	if(length.out > 2L) # not clear why these have as.vector, and not others
	    if(from == to) rep.int(from, length.out)
	    else { # *only* place we could (and did) use 'by's formal default
		n1 <- length.out - 1L
		## integer if "easy"
		if(intdel && intn1 && from %% n1 == to %% n1) {
		    by <- to %/% n1 - from %/% n1
		    cumsum(rep.int(c(from, by), c(1L, n1)))
		}
		else {
		    if (intdel) storage.mode(from) <- "double"
		    by <- (to - from) / n1
		as.vector(c(from, from + seq_len(length.out - 2L) * by, to))
		}
	    }
	else as.vector(c(from, to))[seq_len(length.out)]
    }
    else if(missing(to)) {
	int <- (intby <- is.logint(by)) &&
	    is.logint(from) &&
	    (!(nby <- length(by)) || (naby <- is.na(by)) ||
	     ((to <- from + (length.out - 1) * by) <= .Machine$integer.max &&
	      to >= -.Machine$integer.max))
	if(int && length.out > 2L && nby == 1L && !naby)
	    cumsum(rep.int(c(from, by), c(1L, length.out - 1L)))
	else {
	    if(intby && !(int || is.object(from))) storage.mode(by) <- "double"
	from + (0L:(length.out - 1L)) * by
	}
    }
    else if(missing(from)) {
	int <- (intby <- is.logint(by)) &&
	    is.logint(to) &&
	    (!(nby <- length(by)) || (naby <- is.na(by)) ||
	     ((from <- to - (length.out - 1) * by) >= -.Machine$integer.max &&
	      from <= .Machine$integer.max))
	if(int && length.out > 2L && nby == 1L && !naby)
	    cumsum(rep.int(c(as.integer(from), by), c(1L, length.out - 1L)))
	else {
	    if(intby && !(int || is.object(to))) storage.mode(by) <- "double"
	to - ((length.out - 1L):0L) * by
	}
    }
    else stop("too many arguments")
}

## In reverence to the very first versions of R which already had sequence():
sequence <- function(nvec, ...) UseMethod("sequence")

sequence.default <- function(nvec, from = 1L, by = 1L, ...) {
    .Internal(sequence(as.integer(nvec), as.integer(from), as.integer(by)))
}
