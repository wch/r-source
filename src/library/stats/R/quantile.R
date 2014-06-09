#  File src/library/stats/R/quantile.R
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

quantile <- function(x, ...) UseMethod("quantile")

quantile.POSIXt <- function(x, ...)
    .POSIXct(quantile(unclass(as.POSIXct(x)), ...), attr(x, "tzone"))

quantile.default <-
    function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE,
             type = 7, ...)
{
    if(is.factor(x)) {
        if(!is.ordered(x) || ! type %in% c(1L, 3L))
            stop("factors are not allowed")
        lx <- levels(x)
    } else lx <- NULL
    if (na.rm)
	x <- x[!is.na(x)]
    else if (anyNA(x))
	stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    eps <- 100*.Machine$double.eps
    if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 1+eps)))
	stop("'probs' outside [0,1]")
    n <- length(x)
    if(na.p <- any(!p.ok)) { # set aside NA & NaN
        o.pr <- probs
        probs <- probs[p.ok]
        probs <- pmax(0, pmin(1, probs)) # allow for slight overshoot
    }
    np <- length(probs)
    if (n > 0 && np > 0) {
        if(type == 7) { # be completely back-compatible
            index <- 1 + (n - 1) * probs
            lo <- floor(index)
            hi <- ceiling(index)
            x <- sort(x, partial = unique(c(lo, hi)))
            qs <- x[lo]
	    i <- which(index > lo)
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
			    (nppm > j),		# type 1
			    ((nppm > j) + 1)/2, # type 2
			    (nppm != j) | ((j %% 2L) == 1L)) # type 3
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
                if(any(sml <- abs(h) < fuzz)) h[sml] <- 0
            }
            x <- sort(x, partial =
                      unique(c(1, j[j>0L & j<=n], (j+1)[j>0L & j<n], n))
                      )
            x <- c(x[1L], x[1L], x, x[n], x[n])
            ## h can be zero or one (types 1 to 3), and infinities matter
####        qs <- (1 - h) * x[j + 2] + h * x[j + 3]
            ## also h*x might be invalid ... e.g. Dates and ordered factors
            qs <- x[j+2L]
            qs[h == 1] <- x[j+3L][h == 1]
            other <- (0 < h) & (h < 1)
            if(any(other)) qs[other] <- ((1-h)*x[j+2L] + h*x[j+3L])[other]
        }
    } else {
	qs <- rep(NA_real_, np)
    }
    if(is.character(lx))
        qs <- factor(qs, levels = seq_along(lx), labels = lx, ordered = TRUE)
    if(names && np > 0L) {
	dig <- max(2L, getOption("digits"))
	names(qs) <- paste0(## formatC is slow for long probs
                            if(np < 100) formatC(100*probs, format = "fg", width = 1, digits = dig, decimal.mark = getOption("OutDec"))
                            else format(100 * probs, trim = TRUE, digits = dig),
                            "%")
    }
    if(na.p) { # do this more elegantly (?!)
        o.pr[p.ok] <- qs
        names(o.pr) <- rep("", length(o.pr)) # suppress <NA> names
        names(o.pr)[p.ok] <- names(qs)
        o.pr
    } else qs
}

IQR <- function (x, na.rm = FALSE, type = 7)
    diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm=na.rm, names = FALSE,
		  type = type))
