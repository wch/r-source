splinefun <- function(x, y=NULL, method="fmm")
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    n <- length(x)# = length(y), ensured by xy.coords(.)
    method <- match(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("splinefun: invalid interpolation method")
    if(any(diff(x) < 0)) {
	z <- order(x)
	x <- x[z]
	y <- y[z]
    }
    if(method == 1 && y[1] != y[n]) {
	warning("first and last y values differ in spline - using y[1] for both")
	y[n] <- y[1]
    }
    z <- .C("spline_coef",
	    method=as.integer(method),
	    n=n,
	    x=x,
	    y=y,
	    b=double(n),
	    c=double(n),
	    d=double(n),
	    e=double(if(method == 1) n else 0),
            PACKAGE="base")
    rm(x,y,n,method)
    function(x) {
	.C("spline_eval",
	   z$method,
	   length(x),
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
