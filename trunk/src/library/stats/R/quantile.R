#  File src/library/stats/R/quantile.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

quantile <- function(x, ...) UseMethod("quantile")

quantile.POSIXt <- function(x, ...)
    .POSIXct(quantile(unclass(as.POSIXct(x)), ...), attr(x, "tzone"))

quantile.default <-
    function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE,
             type = 7, digits = 7, ...)
{
    if(is.factor(x)) {
	if(is.ordered(x)) {
	   if(!any(type == c(1L, 3L)))
	       stop("'type' must be 1 or 3 for ordered factors")
	} else
            stop("(unordered) factors are not allowed")
        lx <- levels(x)
        x <- as.integer(x)
    } else {
        if(is.null(x)) x <- numeric() # for back-compatibility
        lx <- NULL
    }
    if (na.rm)
	x <- x[!is.na(x)]
    else if (anyNA(x))
	stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    eps <- 100*.Machine$double.eps
    if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 1+eps)))
	stop("'probs' outside [0,1]")
    n <- length(x)
    probs <- pmax(0, pmin(1, probs)) # allow for slight overshoot
    np <- length(probs)
    {
        if(type == 7) { # be completely back-compatible
            index <- 1 + max(n - 1, 0) * probs
            lo <- floor(index)
            hi <- ceiling(index)
            x <- sort(x, partial = if(n == 0) numeric() else unique(c(lo, hi)[p.ok]))
            qs <- x[lo]
	    i <- which(!p.ok | (index > lo & x[hi] != qs))# '!=' for '>' working w/ complex
	    h <- (index - lo)[i] # > 0	by construction
##	    qs[i] <- qs[i] + .minus(x[hi[i]], x[lo[i]]) * (index[i] - lo[i])
##	    qs[i] <- ifelse(h == 0, qs[i], (1 - h) * qs[i] + h * x[hi[i]])
	    qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
        } else {
            if (type <= 3) {
                ## Types 1, 2 and 3 are discontinuous sample qs.
                nppm <- if (type == 3) n * probs - .5 # n * probs + m; m = -0.5
                        else n * probs          # m = 0
                j <- floor(nppm)
		h <- switch(type,
			    !p.ok | (nppm > j),	# type 1
			    ((nppm > j) + 1)/2, # type 2
			    !p.ok | (nppm != j) | ((j %% 2L) == 1L)) # type 3
            } else {
                ## Types 4 through 9 are continuous sample qs.
                switch(type - 3,
                   {a <- 0; b <- 1},    # type 4
                       a <- b <- 0.5,   # type 5
                       a <- b <- 0,     # type 6
                       a <- b <- 1,     # type 7 (unused here)
                       a <- b <- 1 / 3, # type 8
                       a <- b <- 3 / 8) # type 9
                ## need to watch for rounding errors here
                fuzz <- 4 * .Machine$double.eps
                nppm <- a + probs * (n + 1 - a - b) # n*probs + m
                j <- floor(nppm + fuzz) # m = a + probs*(1 - a - b)
                h <- nppm - j
                if(any(sml <- abs(h) < fuzz, na.rm = TRUE)) h[sml] <- 0
            }
            x <- sort(x, partial =
                      if(n == 0) numeric() else
                      unique(c(1, j[p.ok & j>0L & j<=n], (j+1)[p.ok & j>0L & j<n], n))
                      )
            x <- c(x[1L], x[1L], x, x[n], x[n])
            ## h can be zero or one (types 1 to 3), and infinities matter: more careful than
            ##        qs <- (1 - h) * x[j + 2] + h * x[j + 3]
            ## also h*x might be invalid ... e.g. Dates and ordered factors
            qs <- x[j+2L]
            qs[!is.na(h) & h == 1] <- x[j+3L][!is.na(h) & h == 1]
	    other <- (0 < h) & (h < 1) & (x[j+2L] != x[j+3L]) # '!=' for '<' in complex case
	    other[is.na(other)] <- TRUE
            if(any(other)) qs[other] <- ((1-h)*x[j+2L] + h*x[j+3L])[other]
        }
    }
    if(is.character(lx))
        qs <- factor(qs, levels = seq_along(lx), labels = lx, ordered = TRUE)
    if(names && np > 0L) {
	stopifnot(is.numeric(digits), digits >= 1)
	names(qs) <- format_perc(probs, digits=digits)
    }
    qs
}

##' Formatting() percentages the same way as quantile(*, names=TRUE).
##' Should be exported
##' (and format.pval() moved to stats; both documented on same page)
format_perc <- function(x, digits = max(2L, getOption("digits")),
			probability = TRUE, use.fC = length(x) < 100, ...)
{
    if(length(x)) {
	if(probability) x <- 100 * x
	ans <-
	paste0(if(use.fC) ## formatC is slow for long x
		   formatC(x, format = "fg", width = 1, digits=digits)
	       else format(x, trim = TRUE, digits=digits, ...), "%")
	ans[is.na(x)] <- "" # suppress NA% or NaN%
	ans
    } else character(0)
}

IQR <- function (x, na.rm = FALSE, type = 7)
    diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm=na.rm, names = FALSE,
		  type = type))
