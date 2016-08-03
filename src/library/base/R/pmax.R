#  File src/library/base/R/pmax.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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
    i4 <- isS4(elts[[1L]])
    if(!i4 && all(vapply(elts, function(x) is.atomic(x) && !is.object(x), NA))) {
	## NB: NULL passes is.atomic
	mmm <- .Internal(pmax(na.rm, ...))
    } else {
	mmm <- as.vector(elts[[1L]]) # attr(mmm, "dim") <- NULL  # dim<- would drop names
	has.na <- FALSE
	for (each in elts[-1L]) {
	    each <- as.vector(each) # attr(each, "dim") <- NULL
	    l1 <- length(each); l2 <- length(mmm)
	    if(l2 < l1) {
		if (l2 && l1 %% l2)
		    warning("an argument will be fractionally recycled")
		mmm <- rep(mmm, length.out = l1)
	    } else if(l1 && l1 < l2) {
		if (l2 %% l1)
		    warning("an argument will be fractionally recycled")
		each <- rep(each, length.out = l2)
	    }
	    nas <- cbind(is.na(mmm), is.na(each))
	    if(has.na || (has.na <- any(nas))) {
		mmm [nas[, 1L]] <- each[nas[, 1L]]
		each[nas[, 2L]] <- mmm [nas[, 2L]]
	    }
	    change <- mmm < each
	    change <- change & !is.na(change)
	    mmm[change] <- each[change]
	    if (has.na && !na.rm) mmm[nas[, 1L] | nas[, 2L]] <- NA
	}
    }
    if(i4) {
	r <- elts[[1L]] # i.e. a valid S4 object
	tryCatch({ r[] <- mmm; r }, error = mmm)
    }
    else { # !isS4(.) ; to improve for S3 ?
	mostattributes(mmm) <- attributes(elts[[1L]])
	mmm
    }
}

pmin <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0L) stop("no arguments")
    i4 <- isS4(elts[[1L]])
    if(!i4 && all(vapply(elts, function(x) is.atomic(x) && !is.object(x), NA))) {
	mmm <- .Internal(pmin(na.rm, ...))
    } else {
	mmm <- as.vector(elts[[1L]]) # attr(mmm, "dim") <- NULL  # dim<- would drop names
	has.na <- FALSE
	for (each in elts[-1L]) {
	    each <- as.vector(each) # attr(each, "dim") <- NULL
	    l1 <- length(each); l2 <- length(mmm)
	    if(l2 < l1) {
		if (l2 && l1 %% l2)
		    warning("an argument will be fractionally recycled")
		mmm <- rep(mmm, length.out = l1)
	    } else if(l1 && l1 < l2) {
		if (l2 %% l1)
		    warning("an argument will be fractionally recycled")
		each <- rep(each, length.out = l2)
	    }
	    nas <- cbind(is.na(mmm), is.na(each))
	    if(has.na || (has.na <- any(nas))) {
		mmm [nas[, 1L]] <- each[nas[, 1L]]
		each[nas[, 2L]] <- mmm [nas[, 2L]]
	    }
	    change <- mmm > each
	    change <- change & !is.na(change)
	    mmm[change] <- each[change]
	    if (has.na && !na.rm) mmm[nas[, 1L] | nas[, 2L]] <- NA
	}
    }
    if(i4) {
	r <- elts[[1L]] # i.e. a valid S4 object
	tryCatch({ r[] <- mmm; r }, error = mmm)
    }
    else { # !isS4(.) ; to improve for S3 ?
	mostattributes(mmm) <- attributes(elts[[1L]])
	mmm
    }
}
