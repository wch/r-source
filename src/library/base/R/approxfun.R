### Note if you change: ./approx.R  is very close to this
approxfun <-
    function(x, y = NULL, method = "linear",
	     yleft, yright, rule = 1, f = 0, ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    if (!is.numeric(x) || !is.numeric(y))
	stop("x and y must be numeric")
    n <- length(x)
    if (n != length(y))
	stop("x and y must have equal lengths")
    if (n < 2)
	stop("approx requires at least two values to interpolate")
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	n <- length(x)
    }
    if (!is.character(ties) || ties != "ordered") {
	if (length(ux <- unique(x)) < n) {
	    if (missing(ties))
		warning("Collapsing to unique x values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    n <- length(x)
	    rm(ux)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if (n < 2)
	stop("need at least two unique non-missing values to interpolate")
    if (missing(yleft))
	yleft <- if(rule == 1) NA else y[1]
    if (missing(yright))
	yright <- if(rule == 1) NA else y[length(y)]
    rm(o, rule)
    function(v) .C("R_approx", as.double(x), as.double(y),
		   n, xout = as.double(v), length(v), as.integer(method),
		   as.double(yleft), as.double(yright),
		   as.double(f), NAOK = TRUE, PACKAGE = "base")$xout
}
