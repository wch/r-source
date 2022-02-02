#  File src/library/stats/R/nlm.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

nlm <- function(f, p, ..., hessian=FALSE, typsize=rep(1,length(p)),
		fscale=1, print.level=0, ndigit=12, gradtol=1e-6,
		stepmax=max(1000 * sqrt(sum((p/typsize)^2)), 1000),
		steptol=1e-6, iterlim=100, check.analyticals=TRUE)
{

    print.level <- as.integer(print.level)
    if(print.level < 0 || print.level > 2)
	stop("'print.level' must be in {0,1,2}")
    ## msg is collection of bits, i.e., sum of 2^k (k = 0,..,4):
    msg <- (1 + c(8,0,16))[1+print.level]
    if(!check.analyticals) msg <- msg + (2 + 4)
    .External2(C_nlm, function(x) f(x, ...), p, hessian, typsize, fscale,
               msg, ndigit, gradtol, stepmax, steptol, iterlim)
}

optimize <- function(f, interval, ...,
		     lower=min(interval), upper=max(interval),
		     maximum=FALSE, tol=.Machine$double.eps^0.25)
{
    if(maximum) {
	val <- .External2(C_do_fmin,function(arg) -f(arg, ...), lower, upper, tol)
	list(maximum = val, objective = f(val, ...))
    } else {
	val <- .External2(C_do_fmin, function(arg) f(arg, ...), lower, upper, tol)
	list(minimum = val, objective = f(val, ...))
    }
}

##nice to the English (or rather the Scots)
optimise <- optimize

## FIXME? have the  4 cases
##    Sig \in {NULL,    -1 ,     0,     1  } -- with default 0 --->
## extendInt  ( yes,  downX,    no,    upX ) -- with default "no"
## crossing   (extend, down,   free,   up  ) -- with default "free"
uniroot <- function(f, interval, ...,
		    lower = min(interval), upper = max(interval),
		    f.lower = f(lower, ...), f.upper = f(upper, ...),
		    extendInt = c("no", "yes", "downX", "upX"),
		    check.conv = FALSE,
		    tol = .Machine$double.eps^0.25, maxiter = 1000, trace = 0)
{
    if(!missing(interval) && length(interval) != 2L)
        stop("'interval' must be a vector of length 2")
    if(!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
        stop("lower < upper  is not fulfilled")
    if(is.na(f.lower)) stop("f.lower = f(lower) is NA")
    if(is.na(f.upper)) stop("f.upper = f(upper) is NA")
    Sig <- switch(match.arg(extendInt),
		  "yes" = NULL,
		  "downX"= -1,
		  "no"   =  0,
		  "upX"  =  1,
		  stop("invalid 'extendInt'; please report"))
    ## protect against later   0 * Inf  |--> NaN  and Inf * -Inf.
    truncate <- function(x) pmax.int(pmin(x, .Machine$double.xmax),
                                    -.Machine$double.xmax)
    f.low. <- truncate(f.lower)
    f.upp. <- truncate(f.upper)
    doX <- (   is.null(Sig) && f.low. * f.upp. > 0 ||
	    is.numeric(Sig) && (Sig*f.low. > 0 || Sig*f.upp. < 0))
    if(doX) { ## extend the interval = [lower, upper]
	if(trace)
	    cat(sprintf("search in [%g,%g]%s", lower, upper,
			if(trace >= 2)"\n" else " ... "))
	Delta <- function(u) 0.01* pmax(1e-4, abs(u))
        it <- 0L
	## Two cases:
	if(is.null(Sig)) {
	    ## case 1)	'Sig' unspecified --> extend (lower, upper) at the same time
	    delta <- Delta(c(lower,upper))
	    while(isTRUE(f.lower*f.upper > 0) &&
                  any(iF <- is.finite(c(lower,upper)))) {
		if((it <- it + 1L) > maxiter)
		    stop(gettextf("no sign change found in %d iterations", it-1),
			 domain=NA)
		if(iF[1]) {
		    ol <- lower; of <- f.lower
		    if(is.na(f.lower <- f(lower <- lower - delta[1], ...))) {
			lower <- ol; f.lower <- of; delta[1] <- delta[1]/4
		    }
		}
		if(iF[2]) {
		    ol <- upper; of <- f.upper
		    if(is.na(f.upper <- f(upper <- upper + delta[2], ...))) {
			upper <- ol; f.upper <- of; delta[2] <- delta[2]/4
		    }
		}
		if(trace >= 2)
		    cat(sprintf(" .. modified lower,upper: (%15g,%15g)\n",
				lower,upper))
		delta <- 2 * delta
	    }
	} else {
	    ## case 2) 'Sig' specified --> typically change only *one* of lower, upper
	    ## make sure we have Sig*f(lower) <= 0 and Sig*f(upper) >= 0:
	    delta <- Delta(lower)
	    while(isTRUE(Sig*f.lower > 0)) {
		if((it <- it + 1L) > maxiter)
		    stop(gettextf("no sign change found in %d iterations", it-1),
			 domain=NA)
		f.lower <- f(lower <- lower - delta, ...)
		if(trace >= 2) cat(sprintf(" .. modified lower: %g\n", lower))
		delta <- 2 * delta
	    }
	    delta <- Delta(upper)
	    while(isTRUE(Sig*f.upper < 0)) {
		if((it <- it + 1L) > maxiter)
		    stop(gettextf("no sign change found in %d iterations", it-1),
			 domain=NA)
		f.upper <- f(upper <- upper + delta, ...)
		if(trace >= 2) cat(sprintf(" .. modified upper: %g\n", upper))
		delta <- 2 * delta
	    }
	}
	if(trace && trace < 2)
            cat(sprintf("extended to [%g, %g] in %d steps\n", lower, upper, it))
    }
    ## this might have names
    if(!isTRUE(as.vector(sign(f.lower) * sign(f.upper) <= 0)))
	stop(if(doX)
	"did not succeed extending the interval endpoints for f(lower) * f(upper) <= 0"
	     else "f() values at end points not of opposite sign")

    if(check.conv) {
	val <- tryCatch(.External2(C_zeroin2, function(arg) f(arg, ...),
				   lower, upper, f.lower, f.upper,
				   tol, as.integer(maxiter)),
			warning = function(w)w)
	if(inherits(val, "warning"))
	    stop("convergence problem in zero finding: ", conditionMessage(val))
    } else {
	val <- .External2(C_zeroin2, function(arg) f(arg, ...),
			  lower, upper, f.lower, f.upper,
			  tol, as.integer(maxiter))
    }
    iter <- as.integer(val[2L])
    if(iter < 0) {
	(if(check.conv) stop else warning)(
	    sprintf(ngettext(maxiter,
			     "_NOT_ converged in %d iteration",
			     "_NOT_ converged in %d iterations"),
		    maxiter), domain = NA)
	iter <- maxiter
    }
    if(doX) iter <- iter + it else it <- NA_integer_
    list(root = val[1L], f.root = f(val[1L], ...),
	 iter = iter, init.it = it, estim.prec = val[3L])
}## uniroot()

