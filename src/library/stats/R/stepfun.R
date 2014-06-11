#  File src/library/stats/R/stepfun.R
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

## Constructor for  Step Functions:

## Given x[1L] .. x[n] ;	 y[0] .. y[n]  (one value more !)
## For 'cadlag' functions :  f(t) = y[i]  iff  t in  ( x[i], x[i+1] ],
##     where  x[0] := - Inf
##
## 'General case':  f(x[i]) = z[i]  with arbitrary z[]
## -- but we would have to modify 'approxfun' or not be able to use it..
## --->> forget about general case
stepfun <-
    function(x, y, f = as.numeric(right), ties = "ordered", right = FALSE)
{
    if(is.unsorted(x)) stop("stepfun: 'x' must be ordered increasingly")
    n <- length(x)
    if(n < 1) stop("'x' must have length >= 1")
    n1 <- n + 1L
    if(length(y) != n1) stop("'y' must be one longer than 'x'")
    rval <- approxfun(x, y[- if(right)n1 else 1], method = "constant",
		      yleft = y[1L], yright = y[n1], f = f, ties = ties)
    class(rval) <- c("stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

is.stepfun <- function(x) is.function(x) && inherits(x, "stepfun")

as.stepfun <- function(x, ...) UseMethod("as.stepfun")
as.stepfun.default <- function(x, ...)
{
    if(is.stepfun(x)) x
    else stop("no 'as.stepfun' method available for 'x'")
}

## Quite obvious  that I will want to have  knots.spline(..)  etc......
knots         <- function(Fn, ...) UseMethod("knots")
knots.stepfun <- function(Fn, ...) eval(expression(x), envir=environment(Fn))


print.stepfun <- function (x, digits = getOption("digits") - 2, ...)
{
    numform <- function(x) paste(formatC(x, digits = digits), collapse=", ")
    i1 <- function(n) 1L:min(3L, n)
    i2 <- function(n) if(n >= 4L) max(4L, n-1L):n else integer()
    cat("Step function\nCall: ")
    print(attr(x, "call"), ...)
    env <- environment(x)
    n <- length(xx <- eval(expression(x), envir = env))
    cat(" x[1:", n, "] = ", numform(xx[i1(n)]),
	if(n > 3L) ", ", if(n > 5L) " ..., ", numform(xx[i2(n)]), "\n", sep = "")
    y <- eval(expression(c(yleft, y)), envir = env)
    cat(n+1L, " plateau levels = ", numform(y[i1(n+1L)]),
	if(n+1L > 3L) ", ", if(n+1L > 5L) " ..., ", numform(y[i2(n+1L)]), "\n",
	sep = "")
    invisible(x)
}

summary.stepfun <- function(object, ...)
{
    n <- length(eval(expression(x), envir = environment(object)))
    if(!is.integer(n) || n < 1L) stop("not a valid step function")
    cat("Step function with continuity 'f'=",
	format(eval(expression(f), envir = environment(object))),
	", ", n, if(n <= 6L) "knots at\n" else "knots with summary\n")
    summ <- if(n > 6L) summary else function(x) x
    print(summ(knots(object)))
    cat(if(n > 6L) "\n" else "  ", "and	", n+1L,
        " plateau levels (y) ", if(n <= 6L) "at\n" else "with summary\n",
        sep  = "")
    print(summ(eval(expression(c(yleft,y)), envir = environment(object))))
    invisible()
}

## Purpose: plot method for  stepfun (step function) objects
## --------------------------------------------------------------------
## Arguments: for numeric 'x', do empirical CDF;	  ==> `` ?plot.step ''
## --------------------------------------------------------------------
## Author: Martin Maechler <maechler@stat.math.ethz.ch>
##	      1990, U.Washington, Seattle; improved, Dec.1993
##	      Ported to R :  Sept.1997.
plot.stepfun <-
    function(x, xval, xlim, ylim = range(c(y,Fn.kn)),
	     xlab = "x", ylab = "f(x)", main = NULL,
	     add = FALSE, verticals = TRUE, do.points = (n < 1000),
	     pch = par("pch"), col = par("col"),
             col.points = col, cex.points = par("cex"),
	     col.hor = col, col.vert = col,
	     lty = par("lty"), lwd = par("lwd"),
	     ...)
{
    if(!is.stepfun(x)) { #- make it work when called explicitly with data
	if(is.numeric(x)) {
	    sarg <- substitute(x)
	    x <- ecdf(x)
	    attr(x,"call") <- call("ecdf", sarg)
	} else stop("'plot.stepfun' called with wrong type of argument 'x'")
    }
    if(missing(main))
	main <- {
	    cl <- attr(x,"call")
	    deparse(if(!is.null(cl))cl else sys.call())
	}

    knF <- knots(x)
    xval <- if (missing(xval)) knF else sort(xval)
    if (missing(xlim)) {
        rx <- range(xval)
        dr <-
            if(length(xval) > 1L)
                max(0.08 * diff(rx), median(diff(xval)))
            else
                abs(xval)/16
        xlim <- rx +  dr * c(-1,1)

    } else dr <- diff(xlim)

    xval <- xval[xlim[1L]-dr <= xval & xval <= xlim[2L]+dr]

    ## Careful for heights of horizontals -- these depend on f
    ti <- c(xlim[1L]-dr, xval, xlim[2L]+dr)
    ti.l <- ti[-length(ti)]
    ti.r <- ti[-1L]
    y <- x(0.5*(ti.l + ti.r))
    n <- length(y)
    Fn.kn <- x(xval)

    ##------------------------ Plotting ----------------------------

    dev.hold(); on.exit(dev.flush())
    ## horizontal segments
    if (add)
	segments(ti.l, y, ti.r, y, col=col.hor, lty=lty, lwd=lwd, ...)
    else {
        if(missing(ylim)) ylim <- range(c(y,Fn.kn))
	plot(NA, NA, type = "n", xlim = xlim, ylim = ylim,
	     xlab = xlab, ylab = ylab, main = main, ...)
	segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, lwd = lwd)
    }
    if(do.points)
	points(xval, Fn.kn, pch = pch, col = col.points, cex = cex.points)

    if(verticals)
	segments(xval, y[-n], xval, y[-1L], col = col.vert, lty = lty, lwd = lwd)
    invisible(list(t = ti, y = y))
}

lines.stepfun <- function(x, ...) plot(x, add = TRUE, ...)

as.stepfun.isoreg <- function(x, ...)
{
    sf <- stepfun(x = (if(x$isOrd) x$x else x$x[x$ord])[x$iKnots],
                        y = c(x$yf[x$iKnots], x$yf[length(x$yf)]),
                  right = TRUE)
    attr(sf, "call") <- x$call
    sf
}
