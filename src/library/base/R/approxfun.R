approxfun <- function (x, y=NULL, method = "linear", yleft, yright, rule=1, f=0)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    if (!is.numeric(x) || !is.numeric(y))
	stop("approx: x and y must be numeric")
    n <- length(x)
    if (n != length(y))
	stop("x and y must have equal lengths")
    if (n < 2)
	stop("approx requires at least two values to interpolate")
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("Invalid interpolation method")
    ok <- !(is.na(x) | is.na(y))
    x <- x[ok]
    y <- y[ok]
    o <- order(x)
    x <- x[o]
    y <- y[o]
    if (missing(yleft))
	yleft <- if(rule == 1) NA else y[1]
    if (missing(yright))
	yright <- if(rule == 1) NA else y[length(y)]
    rm(o, ok, rule)
    function(v) .C("R_approx", as.double(x), as.double(y),
		   n, xout = as.double(v), length(v), as.integer(method),
		   as.double(yleft), as.double(yright),
		   as.double(f), NAOK=TRUE, PACKAGE="base")$xout
}
