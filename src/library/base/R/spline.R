spline <- function(x, y=NULL, n=3*length(x), method="fmm", xmin=min(x), xmax=max(x))
{
	x <- xy.coords(x, y)
	y <- x$y
	x <- x$x
	if (!is.numeric(x) || !is.numeric(y))
		stop("spline: x and y must be numeric")
	nx <- length(x)
	if (nx != length(y))
		stop("x and y must have equal lengths")
	method <- match(method, c("periodic", "natural", "fmm"))
	if(is.na(method))
		stop("spline: invalid interpolation method")
##<TSL>	if(any(diff(x) <= 0))
##		stop("invalid x array in spline")
	o<-order(x)
	x<-x[o]
	y<-y[o]
	if(method == 1 && y[1] != y[nx]) {
		warning("spline: first and last y values differ - using y[1] for both")
		y[nx] <- y[1]
	}
	z <- .C("spline_coef",
		method=as.integer(method),
		n=nx,
		x=as.double(x),
		y=as.double(y),
		b=double(nx),
		c=double(nx),
		d=double(nx),
		e=double(if(method == 1) nx else 0))
	u <- seq(xmin, xmax, length.out=n)
	.C("spline_eval",
		z$method,
		length(u),
		x=u,
		y=double(n),
		z$n,
		z$x,
		z$y,
		z$b,
		z$c,
		z$d)[c("x","y")]
}
