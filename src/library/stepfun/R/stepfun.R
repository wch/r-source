## Constructor for  Step Functions:

## Given x[1] .. x[n] ;	 y[0] .. y[n]  (one value more !)
## For 'cadlag' functions :  f(t) = y[i]  iff  t in  ( x[i], x[i+1] ],
##     where  x[0] := - Inf
##
## 'General case':  f(x[i]) = z[i]  with arbitrary z[]
## -- but we would have to modify 'approxfun' or not be able to use it..
## --->> forget about general case
stepfun <- function (x, y, f = 0, ties = "ordered")
{
    if(is.unsorted(x)) stop("stepfun: x must be ordered increasingly")
    n <- length(x)
    if(length(y)!= n+1) stop("y must be one longer than x")
    rval <- approxfun(x, y[-1], method = "constant",
		      yleft = y[1], yright = y[n+1], f = f, ties = ties)
    class(rval) <- c("stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

is.stepfun <- function(x) is.function(x) && inherits(x, "stepfun")

## Quite obvious  that I will want to have  knots.spline(..)  etc......
knots <- function(Fn, ...) UseMethod("knots")
knots.stepfun <- function(Fn, ...) eval(expression(x), env=environment(Fn))


print.stepfun <- function (Fn, digits = getOption("digits") - 2, ...)
{
    numform <- function(x) paste(formatC(x, dig = digits), collapse=", ")
    i1 <- function(n) 1:min(3, n)
    i2 <- function(n) if(n >= 4) max(4, n-1):n else integer(0)
    cat("Step function\nCall: ")
    print(attr(Fn, "call"), ...)
    env <- environment(Fn)
    n <- length(x <- eval(expression(x), env = env))
    cat(" x[1:", n, "] = ", numform(x[i1(n)]),
	if(n > 3) ", ", if(n > 5) " ..., ", numform(x[i2(n)]), "\n", sep = "")
    y <- eval(expression(c(yleft, y)), env = env)
    cat(n+1, " step heights = ", numform(y[i1(n+1)]),
	if(n+1 > 3) ", ", if(n+1 > 5) " ..., ", numform(y[i2(n+1)]), "\n",
	sep = "")
    invisible(Fn)
}

summary.stepfun <- function(Fn){
    n <- eval(expression(n),env = environment(Fn))
    if(!is.integer(n) || n < 1) stop("not a valid step function")
    ## n <- n-1
    cat("Step function with continuity 'f'=",
	format(eval(expression(f),env = environment(Fn))),
	", ", n, "knots at\n")
    summ <- if(n>6) summary else function(x) x
    print(summ(knots(Fn)))
    cat(if(n>6)"\n" else"  ", "and	", n+1," step heights (y) at\n", sep="")
    print(summ(eval(expression(c(yleft,y)),env = environment(Fn))))
    invisible()
}

## Purpose: plot method for  stepfun (step function) objects
## --------------------------------------------------------------------
## Arguments: for numeric Fn, do empirical CDF;	  ==> `` ?plot.step ''
## --------------------------------------------------------------------
## Author: Martin Maechler <maechler@stat.math.ethz.ch>
##	      1990, U.Washington, Seattle; improved, Dec.1993
##	      Ported to R :  Sept.1997.
plot.stepfun <-
    function(Fn, xval, xlim, xlab = "x", ylab = "f(x)", main = NULL,
             add = FALSE, verticals = TRUE, do.points = TRUE,
	     pch = par("pch"), col.points=par("col"), cex.points=par("cex"),
	     col.hor = par("col"), col.vert= par("col"),
             lty = par("lty"), lwd = par("lwd"),
             ...)
{
    if(!is.stepfun(Fn)) { #- make it work when called explicitly with data
	if(is.numeric(Fn)) {
	    sarg <- substitute(Fn)
	    Fn <- ecdf(Fn)
	    attr(Fn,"call") <- call("ecdf", sarg)
	} else stop("plot.stepfun called with wrong argument `Fn'")
    }
    if(missing(main))
	main <- {
	    cl <- attr(Fn,"call")
	    deparse(if(!is.null(cl))cl else sys.call())
	}

    knF <- knots(Fn)
    xval <- if (missing(xval)) knF else sort(xval)
    if (missing(xlim)) {
	dr <- diff(rx <- range(xval))
	dr <- max(0.08 * dr,  median(diff(xval)))
	xlim <- rx +  dr * c(-1,1)
    } else dr <- diff(xlim)

    knF <- knF[xlim[1]-dr <= knF & knF <= xlim[2]+dr]

    ## Careful for heights of horizontals -- these depend on f
    ti <- c(xlim[1]-dr, knF, xlim[2]+dr)
    ti.l <-	 ti[-length(ti)]
    ti.r <-	 ti[-1]
    y <- Fn(.5*(ti.l+ti.r))
    n <- length(y)
    Fn.kn <- Fn(knF)

    ##------------------------ Plotting ----------------------------

    ## horizontal segments
    if (add)
	segments(ti.l, y, ti.r, y, col=col.hor, lty=lty, lwd=lwd, ...)
    else {
	plot(0,0, type="n", xlim=xlim, ylim=range(c(y,Fn.kn)),
	     xlab=xlab, ylab=ylab, main= main, ...)
	segments(ti.l, y, ti.r, y, col=col.hor, lty=lty, lwd=lwd)
    }
    if(do.points)
	points(knF, Fn.kn, pch=pch, col=col.points, cex=cex.points)

    if(verticals)
	segments(knF, y[-n], knF, y[-1], col=col.vert, lty=lty, lwd=lwd)
    invisible(list(t = ti, y = y))
}





