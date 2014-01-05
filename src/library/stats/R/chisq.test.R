#  File src/library/stats/R/chisq.test.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

chisq.test <- function(x, y = NULL, correct = TRUE,
		       p = rep(1/length(x), length(x)),
		       rescale.p = FALSE, simulate.p.value = FALSE, B = 2000)
{
    DNAME <- deparse(substitute(x))
    if (is.data.frame(x))
	x <- as.matrix(x)
    if (is.matrix(x)) { # why not just drop()?
	if (min(dim(x)) == 1L)
	    x <- as.vector(x)
    }
    if (!is.matrix(x) && !is.null(y)) {
	if (length(x) != length(y))
	    stop("'x' and 'y' must have the same length")
        DNAME2 <- deparse(substitute(y))
        ## omit names on dims if too long (and 1 line might already be too long)
        xname <- if(length(DNAME) > 1L || nchar(DNAME, "w") > 30) "" else DNAME
        yname <- if(length(DNAME2) > 1L || nchar(DNAME2, "w") > 30) "" else DNAME2
	OK <- complete.cases(x, y)
	x <- factor(x[OK])
	y <- factor(y[OK])
	if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
	    stop("'x' and 'y' must have at least 2 levels")
	## Could also call table() with 'deparse.level = 2', but we need
	## to deparse ourselves for DNAME anyway ...
	x <- table(x, y)
	names(dimnames(x)) <- c(xname, yname)
        ## unclear what to do here: might abbreviating be preferable?
	DNAME <- paste(paste(DNAME, collapse = "\n"),
                       "and",
                       paste(DNAME2, collapse = "\n"))
    }

    if (any(x < 0) || anyNA(x))
	stop("all entries of 'x' must be nonnegative and finite")
    if ((n <- sum(x)) == 0)
	stop("at least one entry of 'x' must be positive")

    if(simulate.p.value) {
	setMETH <- function() # you shalt not cut_n_paste
	    METHOD <<- paste(METHOD,
			     "with simulated p-value\n\t (based on", B,
			     "replicates)")
        almost.1 <- 1 - 64 * .Machine$double.eps
    }
    if (is.matrix(x)) {
	METHOD <- "Pearson's Chi-squared test"
        nr <- as.integer(nrow(x))
        nc <- as.integer(ncol(x))
        if (is.na(nr) || is.na(nc) || is.na(nr * nc))
            stop("invalid nrow(x) or ncol(x)", domain = NA)
	sr <- rowSums(x)
	sc <- colSums(x)
	E <- outer(sr, sc, "*") / n

        ## Cell residual variance. Essentially formula (2.9) in Agresti(2007).
        v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3

        V <- outer(sr, sc, v, n)

	dimnames(E) <- dimnames(x)
	if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
	    setMETH()
            tmp <- .Call(C_chisq_sim, sr, sc, B, E)
	    ## Sorting before summing may look strange, but seems to be
	    ## a sensible way to deal with rounding issues (PR#3486):
	    STATISTIC <- sum(sort((x - E) ^ 2 / E, decreasing = TRUE))
	    PARAMETER <- NA
	    ## use correct significance level for a Monte Carlo test
	    PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC)) / (B + 1)
	}
	else {
	    if (simulate.p.value)
		warning("cannot compute simulated p-value with zero marginals")
	    if (correct && nrow(x) == 2L && ncol(x) == 2L) {
		YATES <- min(0.5, abs(x-E))
                if (YATES > 0)
		    METHOD <- paste(METHOD, "with Yates' continuity correction")
	    }
	    else
		YATES <- 0
	    STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
	    PARAMETER <- (nr - 1L) * (nc - 1L)
	    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
	}
    }
    else {
	if (length(x) == 1L)
	    stop("'x' must at least have 2 elements")
	if (length(x) != length(p))
	    stop("'x' and 'p' must have the same number of elements")
	if(any(p < 0)) stop("probabilities must be non-negative.")
	if(abs(sum(p)-1) > sqrt(.Machine$double.eps)) {
	    if(rescale.p) p <- p/sum(p)
	    else stop("probabilities must sum to 1.")
	}
	METHOD <- "Chi-squared test for given probabilities"
	E <- n * p
        V <- n * p * (1 - p)
	STATISTIC <- sum((x - E) ^ 2 / E)
	names(E) <- names(x)
	if(simulate.p.value) {
	    setMETH()
	    nx <- length(x)
	    sm <- matrix(sample.int(nx, B*n, TRUE, prob = p),nrow = n)
	    ss <- apply(sm, 2L, function(x,E,k) {
		sum((table(factor(x, levels=1L:k)) - E)^2 / E)
	    }, E = E, k = nx)
	    PARAMETER <- NA
	    PVAL <- (1 + sum(ss >= almost.1 * STATISTIC))/(B + 1)
	} else {
	    PARAMETER <- length(x) - 1
	    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
	}
    }

    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    if (any(E < 5) && is.finite(PARAMETER))
	warning("Chi-squared approximation may be incorrect")

    structure(list(statistic = STATISTIC,
		   parameter = PARAMETER,
		   p.value = PVAL,
		   method = METHOD,
		   data.name = DNAME,
		   observed = x,
		   expected = E,
		   residuals = (x - E) / sqrt(E),
                   stdres = (x - E) / sqrt(V) ),
	      class = "htest")
}
