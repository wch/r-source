spline <-
    function(x, y=NULL, n=3*length(x), method="fmm", xmin=min(x), xmax=max(x))
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    method <- pmatch(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("spline: invalid interpolation method")
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
    }
    if(is.unsorted(x)) {
	o <- order(x)
	x <- x[o]
	y <- y[o]
    }
    nx <- length(x)# = length(y), ensured by xy.coords(.)
    if(method == 1 && y[1] != y[nx]) { # periodic
        warning("spline: first and last y values differ - using y[1] for both")
        y[nx] <- y[1]
    }
    if(nx == 0) stop("zero non-NA points")
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
