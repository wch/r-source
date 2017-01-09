#  File src/library/base/R/pmax.R
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

pmin.int <- function(..., na.rm = FALSE) .Internal(pmin(na.rm, ...))
pmax.int <- function(..., na.rm = FALSE) .Internal(pmax(na.rm, ...))

pmax <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0L) stop("no arguments")
    if(all(vapply(elts, function(x) is.atomic(x) && !is.object(x), NA))) { # incl. NULL
	mmm <- .Internal(pmax(na.rm, ...))
	mostattributes(mmm) <- attributes(elts[[1L]])
    } else {
	mmm <- elts[[1L]] ## attr(mmm, "dim") <- NULL  # dim<- would drop names
	has.na <- FALSE
        as <- methods::as
        asL <- function(x) if(isS4(x)) as(x, "logical") else x
	for (each in elts[-1L]) {
	    ## attr(each, "dim") <- NULL ## FIXME: deal with d.fr.s !
	    l1 <- length(each); l2 <- length(mmm)
	    if(l2 && (l2 < l1 || !l1)) {
		if (l1 %% l2)
		    warning("an argument will be fractionally recycled")
		mmm <- rep(mmm, length.out = l1)
	    } else if(l1 && (l1 < l2 || !l2)) {
		if (l2 %% l1)
		    warning("an argument will be fractionally recycled")
		each <- rep(each, length.out = l2)
	    }
	    na.m <- is.na(mmm)
	    na.e <- is.na(each)
	    if(has.na || (has.na <- any(na.m) || any(na.e))) {
		if(any(na.m <- asL(na.m))) mmm [na.m] <- each[na.m]
		if(any(na.e <- asL(na.e))) each[na.e] <- mmm [na.e]
	    }
	    nS4 <- !isS4(mmm)
	    if(isS4(change <- mmm < each) && (nS4 || !isS4(each))) # e.g., keep sparse 'each'
		change <- as(change, "logical")# not as.vector(): kills the d.fr. case
	    change <- change & !is.na(change)
	    mmm[change] <- each[change]
	    if (has.na && !na.rm) mmm[na.m | na.e] <- NA
	    if(nS4) mostattributes(mmm) <- attributes(elts[[1L]])
	}
    }
    mmm
}

pmin <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0L) stop("no arguments")
    if(all(vapply(elts, function(x) is.atomic(x) && !is.object(x), NA))) { # incl. NULL
	mmm <- .Internal(pmin(na.rm, ...))
	mostattributes(mmm) <- attributes(elts[[1L]])
    } else {
	mmm <- elts[[1L]] ## attr(mmm, "dim") <- NULL  # dim<- would drop names
	has.na <- FALSE
        as <- methods::as
        asL <- function(x) if(isS4(x)) as(x, "logical") else x
	for (each in elts[-1L]) {
	    ## attr(each, "dim") <- NULL ## FIXME: deal with d.fr.s !
	    l1 <- length(each); l2 <- length(mmm)
	    if(l2 && (l2 < l1 || !l1)) {
		if (l1 %% l2)
		    warning("an argument will be fractionally recycled")
		mmm <- rep(mmm, length.out = l1)
	    } else if(l1 && (l1 < l2 || !l2)) {
		if (l2 %% l1)
		    warning("an argument will be fractionally recycled")
		each <- rep(each, length.out = l2)
	    }
	    na.m <- is.na(mmm)
	    na.e <- is.na(each)
	    if(has.na || (has.na <- any(na.m) || any(na.e))) {
		if(any(na.m <- asL(na.m))) mmm [na.m] <- each[na.m]
		if(any(na.e <- asL(na.e))) each[na.e] <- mmm [na.e]
	    }
	    nS4 <- !isS4(mmm)
	    if(isS4(change <- mmm > each) && (nS4 || !isS4(each))) # e.g., keep sparse 'each'
		change <- as(change, "logical")# not as.vector(): kills the d.fr. case
	    change <- change & !is.na(change)
	    mmm[change] <- each[change]
	    if (has.na && !na.rm) mmm[na.m | na.e] <- NA
	    if(nS4) mostattributes(mmm) <- attributes(elts[[1L]])
	}
    }
    mmm
}
