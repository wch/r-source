#  File src/library/base/R/RNG.R
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

## Random Number Generator

## The available kinds are in
## ../../../include/Random.h  and ../../../main/RNG.c [RNG_Table]
##
RNGkind <- function(kind = NULL, normal.kind = NULL, sample.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "L'Ecuyer-CMRG", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    s.kinds <- c("Rounding", "Rejection", "default")
    do.set <- length(kind) > 0L
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1L)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1L))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1L) i.knd <- -1L
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) != 1L)
	    stop("'normal.kind' must be a character string of length 1")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if(is.na(normal.kind))
	    stop(gettextf("'%s' is not a valid choice", normal.kind),
                 domain = NA)
	if (normal.kind == 0L)
            warning("buggy version of Kinderman-Ramage generator used",
                    domain = NA)
         if(normal.kind == length(n.kinds) - 1L) normal.kind <- -1L
    }

    if(!is.null(sample.kind)) {
        if(!is.character(sample.kind) || length(sample.kind) != 1L)
            stop("'sample.kind' must be a character string of length 1")
        sample.kind <- pmatch(sample.kind, s.kinds) - 1L
        if(is.na(sample.kind))
            stop(gettextf("'%s' is not a valid choice", sample.kind),
                 domain = NA)
        if (sample.kind == 0L)
            warning("non-uniform 'Rounding' sampler used",
                    domain = NA)
         if(sample.kind == length(s.kinds) - 1L) sample.kind <- -1L
    }
    r <- 1L + .Internal(RNGkind(i.knd, normal.kind, sample.kind))
    r <- c(kinds[r[1L]], n.kinds[r[2L]], s.kinds[r[3L]])
    if(do.set || !is.null(normal.kind) || !is.null(sample.kind))
	invisible(r) else r
}

set.seed <- function(seed, kind = NULL, normal.kind = NULL, sample.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "L'Ecuyer-CMRG", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    s.kinds <- c("Rounding", "Rejection", "default")
    if(length(kind) ) {
	if(!is.character(kind) || length(kind) > 1L)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1L))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1L) i.knd <- -1L
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) != 1L)
	    stop("'normal.kind' must be a character string of length 1")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if(is.na(normal.kind))
	    stop(gettextf("'%s' is not a valid choice", normal.kind),
                 domain = NA)
	if (normal.kind == 0L)
            stop("buggy version of Kinderman-Ramage generator is not allowed",
                 domain = NA)
         if(normal.kind == length(n.kinds) - 1L) normal.kind <- -1L
    }
    if(!is.null(sample.kind)) {
        if(!is.character(sample.kind) || length(sample.kind) != 1L)
            stop("'sample.kind' must be a character string of length 1")
        sample.kind <- pmatch(sample.kind, s.kinds) - 1L
        if(is.na(sample.kind))
            stop(gettextf("'%s' is not a valid choice", sample.kind),
                 domain = NA)
        if (sample.kind == 0L)
            warning("non-uniform 'Rounding' sampler used",
                    domain = NA)
         if(sample.kind == length(s.kinds) - 1L) sample.kind <- -1L
    }
    .Internal(set.seed(seed, i.knd, normal.kind, sample.kind))
}

# Compatibility function to set RNGkind as in a given R version

RNGversion <- function(vstr)
{
    vnum <- as.numeric(strsplit(as.character(vstr), ".", fixed=TRUE)[[1L]])
    if (length(vnum) < 2L)
	stop("malformed version string")
    if (vnum[1L] == 0 && vnum[2L] < 99)
        RNGkind("Wichmann-Hill", "Buggy Kinderman-Ramage", "Rounding")
    else if (vnum[1L] == 0 || vnum[1L] == 1 && vnum[2L] <= 6)
	RNGkind("Marsaglia-Multicarry", "Buggy Kinderman-Ramage", "Rounding")
    else if (vnum[1L] <= 2 || vnum[1L] == 3 && vnum[2L] <= 5)
	RNGkind("Mersenne-Twister", "Inversion", "Rounding")
    else
	RNGkind("Mersenne-Twister", "Inversion", "Rejection")
}
