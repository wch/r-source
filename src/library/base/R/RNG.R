#  File src/library/base/R/RNG.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2026 The R Core Team
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
RNGkind <- function(kind = NULL, normal.kind = NULL, sample.kind = NULL, binom.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "L'Ecuyer-CMRG", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    s.kinds <- c("Rounding", "Rejection", "default")
    b.kinds <- c("Buggy BTPE", "BTPE", "default")
    do.set <- length(kind) > 0L
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1L)
	    stop("'kind' must be a character string (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1L))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1L) i.knd <- -1L
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) != 1L)
	    stop(gettextf("'%s' must be a character string", "normal.kind"),
		 domain = NA)
        normal.kind <- pmatch(normal.kind, n.kinds) - 1L
        if(is.na(normal.kind))
	    stop(gettextf("'%s' is not valid for '%s'", normal.kind, "normal.kind"),
                 domain = NA)
	if (normal.kind == 0L)
            warning("buggy version of Kinderman-Ramage generator used",
                    domain = NA)
	if(normal.kind == length(n.kinds) - 1L) normal.kind <- -1L
    }

    if(!is.null(sample.kind)) {
        if(!is.character(sample.kind) || length(sample.kind) != 1L)
	    stop(gettextf("'%s' must be a character string", "sample.kind"),
		 domain = NA)
        sample.kind <- pmatch(sample.kind, s.kinds) - 1L
        if(is.na(sample.kind))
	    stop(gettextf("'%s' is not valid for '%s'", sample.kind, "sample.kind"),
                 domain = NA)
        if (sample.kind == 0L)
            warning("non-uniform 'Rounding' sampler used",
                    domain = NA)
        if(sample.kind == length(s.kinds) - 1L) sample.kind <- -1L
    }
    if(!is.null(binom.kind)) {
	if(!is.character(binom.kind) || length(binom.kind) != 1L)
	    stop(gettextf("'%s' must be a character string", "binom.kind"),
		 domain = NA)
	binom.kind <- pmatch(binom.kind, b.kinds) - 1L
	if(is.na(binom.kind))
	    stop(gettextf("'%s' is not valid for '%s'", binom.kind, "binom.kind"),
		 domain = NA)
	if (binom.kind == 0L)
	    warning("Buggy BTPE algorithm used for rbinom()",
		    domain = NA)
	if(binom.kind == length(b.kinds) - 1L) binom.kind <- -1L
    }
    r <- 1L + .Internal(RNGkind(i.knd, normal.kind, sample.kind, binom.kind))
    r <- c(kinds[r[1L]], n.kinds[r[2L]], s.kinds[r[3L]], b.kinds[r[4L]])
    if(do.set || !is.null(normal.kind) || !is.null(sample.kind) || !is.null(binom.kind))
	invisible(r) else r
}

## FIXME: modularize the substantial common code into a common auxiliary

set.seed <- function(seed, kind = NULL, normal.kind = NULL, sample.kind = NULL, binom.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "L'Ecuyer-CMRG", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    s.kinds <- c("Rounding", "Rejection", "default")
    b.kinds <- c("Buggy BTPE", "BTPE", "default")
    if(length(kind) ) {
	if(!is.character(kind) || length(kind) > 1L)
	    stop("'kind' must be a character string (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1L))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1L) i.knd <- -1L
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) != 1L)
	    stop(gettextf("'%s' must be a character string", "normal.kind"),
		 domain = NA)
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
	    stop(gettextf("'%s' must be a character string", "sample.kind"),
		 domain = NA)
        sample.kind <- pmatch(sample.kind, s.kinds) - 1L
        if(is.na(sample.kind))
	    stop(gettextf("'%s' is not valid for '%s'", sample.kind, "sample.kind"),
                 domain = NA)
        if (sample.kind == 0L)
            warning("non-uniform 'Rounding' sampler used",
                    domain = NA)
        if(sample.kind == length(s.kinds) - 1L) sample.kind <- -1L
    }
    if(!is.null(binom.kind)) {
        if(!is.character(binom.kind) || length(binom.kind) != 1L)
	    stop(gettextf("'%s' must be a character string", "binom.kind"),
		 domain = NA)
        binom.kind <- pmatch(binom.kind, b.kinds) - 1L
        if(is.na(binom.kind))
	    stop(gettextf("'%s' is not valid for '%s'", binom.kind, "binom.kind"),
                 domain = NA)
        if (binom.kind == 0L)
            warning("Buggy BTPE algorithm used for rbinom()",
                    domain = NA)
        if(binom.kind == length(b.kinds) - 1L) binom.kind <- -1L
    }
    .Internal(set.seed(seed, i.knd, normal.kind, sample.kind, binom.kind))
}

# Compatibility function to set RNGkind as in a given R version

RNGversion <- function(vstr)
{
    vnum <- as.numeric(strsplit(as.character(vstr), ".", fixed=TRUE)[[1L]])
    if (length(vnum) < 2L)
	stop("malformed version string")
    if (vnum[1L] == 0 && vnum[2L] < 99) # R < 1.0.0
	RNGkind("Wichmann-Hill", "Buggy Kinderman-Ramage", "Rounding", "Buggy BTPE")
    else if (vnum[1L] == 0 || vnum[1L] == 1 && vnum[2L] <= 6) # R <= 1.6.z
	RNGkind("Marsaglia-Multicarry", "Buggy Kinderman-Ramage", "Rounding", "Buggy BTPE")
    else if (vnum[1L] <= 2 || vnum[1L] == 3 && vnum[2L] <= 5) # R <= 3.5.z
	RNGkind("Mersenne-Twister", "Inversion", "Rounding", "Buggy BTPE")
    else if (vnum[1L] <= 3 || vnum[1L] == 4 && vnum[2L] <= 6) # R <= 4.6.z
	RNGkind("Mersenne-Twister", "Inversion", "Rejection", "Buggy BTPE")
    else # R > 4.6.z
	RNGkind("Mersenne-Twister", "Inversion", "Rejection", "BTPE")
}
