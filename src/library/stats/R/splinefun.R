splinefun <- function(x, y=NULL, method="fmm")
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    method <- pmatch(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("splinefun: invalid interpolation method")
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
    rm(x,y,nx,o,method)
    function(x) {
	.C("spline_eval",
	   z$method,
	   as.integer(length(x)),
	   x=as.double(x),
	   y=double(length(x)),
	   z$n,
	   z$x,
	   z$y,
	   z$b,
	   z$c,
	   z$d,
	   PACKAGE="base")$y
    }
}
