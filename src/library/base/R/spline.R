spline <-
    function(x, y=NULL, n=3*length(x), method="fmm", xmin=min(x), xmax=max(x))
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    ## ensured by  xy.coords(.) :
    ##	if (!is.numeric(x) || !is.numeric(y))
    ##		stop("spline: x and y must be numeric")
    nx <- length(x)
    ## ensured by  xy.coords(.) :
    ##	if (nx != length(y))
    ##		stop("x and y must have equal lengths")
    method <- match(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("spline: invalid interpolation method")
    dx <- diff(x)
    if(any(dx < 0)) {
	o <- order(x)
	x <- x[o]
	y <- y[o]
    }
    if(method == 1 && y[1] != y[nx]) {
	warning("spline: first and last y values differ - using y[1] for both")
	y[nx] <- y[1]
    }
    z <- .C("spline_coef",
	    method=as.integer(method),
	    n=nx,
	    x=x,
	    y=y,
	    b=double(nx),
	    c=double(nx),
	    d=double(nx),
	    e=double(if(method == 1) nx else 0),
            PACKAGE="base")
    u <- seq(xmin, xmax, length.out=n)
    ##-	 cat("spline(.): result of  .C(\"spline_coef\",...):\n")
    ##-	 str(z, vec.len=10)
    ##-	 cat("spline(.): now calling .C(\"spline_eval\", ...)\n")

    .C("spline_eval",
       z$method,
       nu=length(u),
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


