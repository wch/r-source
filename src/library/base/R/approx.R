### Note if you change: ./approxfun.R  is very close to this
approx <-
    function (x, y = NULL, xout, method = "linear", n = 50,
	      yleft, yright, rule = 1, f = 0, ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    if (!is.numeric(x) || !is.numeric(y))
	stop("x and y must be numeric")
    nx <- length(x)
    if (nx != length(y))
	stop("x and y must have equal lengths")
    if (nx < 2)
	stop("approx requires at least two values to interpolate")
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    if(any(na <- is.na(x) | is.na(y))) {
	ok <- !na
	x <- x[ok]
	y <- y[ok]
	nx <- length(x)
    }
    if (!is.character(ties) || ties != "ordered") {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("Collapsing to unique x values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if (nx < 2)
	stop("need at least two unique non-missing values to interpolate")
    if (missing(yleft))
	yleft <- if (rule == 1) NA else y[1]
    if (missing(yright))
	yright <- if (rule == 1) NA else y[length(y)]
    if (missing(xout)) {
	if (n <= 0)
	    stop("approx requires n >= 1")
	xout <- seq(x[1], x[nx], length = n)
    }
    y <- .C("R_approx", as.double(x), as.double(y), nx, xout = as.double(xout),
	length(xout), as.integer(method), as.double(yleft), as.double(yright),
	as.double(f), NAOK = TRUE, PACKAGE = "base")$xout
    list(x = xout, y = y)
}
