#  File src/library/stats/R/nlm.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

uniroot <- function(f, interval, ...,
                    lower = min(interval), upper = max(interval),
                    f.lower = f(lower, ...), f.upper = f(upper, ...),
		    tol = .Machine$double.eps^0.25, maxiter = 1000)
{
    if(!missing(interval) && length(interval) != 2L)
        stop("'interval' must be a vector of length 2")
    if(!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
        stop("lower < upper  is not fulfilled")
    if(is.na(f.lower)) stop("f.lower = f(lower) is NA")
    if(is.na(f.upper)) stop("f.upper = f(upper) is NA")
    if(f.lower * f.upper > 0)
	stop("f() values at end points not of opposite sign")
    val <- .External2(C_zeroin2, function(arg) f(arg, ...),
                      lower, upper, f.lower, f.upper,
                      tol, as.integer(maxiter))
    iter <- as.integer(val[2L])
    if(iter < 0) {
        warning(sprintf(ngettext(maxiter,
                                 "_NOT_ converged in %d iteration",
                                 "_NOT_ converged in %d iterations"),
                        maxiter), domain = NA)
        iter <- maxiter
    }
    list(root = val[1L], f.root = f(val[1L], ...),
         iter = iter, estim.prec = val[3L])
}
