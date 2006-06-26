#### 'spline' and 'splinefun' are very similar --- keep in sync!
####  also consider ``compatibility'' with  'approx' and 'approxfun'

spline <-
    function(x, y=NULL, n=3*length(x), method="fmm", xmin=min(x), xmax=max(x),
             ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    nx <- length(x)
    method <- pmatch(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("invalid interpolation method")
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	nx <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	    rm(ux)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if(nx == 0) stop("zero non-NA points")
    if(method == 1 && y[1] != y[nx]) { # periodic
        warning("spline: first and last y values differ - using y[1] for both")
        y[nx] <- y[1]
    }
    z <- .C("spline_coef",
	    method=as.integer(method),
	    n=as.integer(nx),
	    x=x,
	    y=y,
	    b=double(nx),
	    c=double(nx),
	    d=double(nx),
	    e=double(if(method == 1) nx else 0),
	    PACKAGE="base")
    u <- seq(xmin, xmax, length.out=n)

    .C("spline_eval",
       z$method,
       nu=as.integer(length(u)),
       x =u,
       y =double(n),
       z$n,
       z$x,
       z$y,
       z$b,
       z$c,
       z$d,
       PACKAGE="base")[c("x","y")]
}
