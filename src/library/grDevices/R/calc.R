# Functions that calculate useful stuff for plotting
# BUT which do not do any actual drawing
# Useful for both graphics and grid to have access to

boxplot.stats <- function(x, coef = 1.5, do.conf=TRUE, do.out=TRUE)
{
    nna <- !is.na(x)
    n <- sum(nna)                       # including +/- Inf
    stats <- stats::fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    if(coef < 0) stop("'coef' must not be negative")
    if(coef == 0)
	do.out <- FALSE
    else {                              # coef > 0
	out <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
	if(any(out[nna])) stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    }
    conf <- if(do.conf) stats[3] + c(-1.58, 1.58) * iqr / sqrt(n)
    list(stats = stats, n = n, conf = conf,
	 out = if(do.out) x[out & nna] else numeric(0))
}

# Contour lines
contourLines <-
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
	  z, nlevels = 10, levels = pretty(range(z, na.rm=TRUE), nlevels))
{
    # FIXME: This "validation" code for the x, y, z values
    # should be put in a function for contourLines, contour,
    # image (and persp?) to share.  Unfortunately, an xyz.coords
    # already exists which isn't really compatible with the
    # desired behaviour here.
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		z <- x
		x <- seq(0, 1, len = nrow(z))
	    }
	} else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
	y <- x$y
	x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
	stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
	stop("no proper 'z' matrix specified")
    ##- don't lose  dim(.)
    if (!is.double(z)) storage.mode(z) <- "double"
    invisible(.Internal(contourLines(as.double(x), as.double(y), z,
                                     as.double(levels))))
}

chull <- function(x, y=NULL)
{
    X <- xy.coords(x, y, recycle = TRUE)
    x <- cbind(X$x, X$y)
    n <- nrow(x)
    if(n == 0) return(integer(0))
    z <- .C(R_chull,
	    n=as.integer(n),
	    as.double(x),
	    as.integer(n),
	    as.integer(1:n),
	    integer(n),
	    integer(n),
	    ih=integer(n),
	    nh=integer(1),
	    il=integer(n))
    rev(z$ih[1:z$nh])
}

nclass.Sturges <- function(x) ceiling(log2(length(x)) + 1)

nclass.scott <- function(x)
{
    h <- 3.5 * sqrt(var(x)) * length(x)^(-1/3)
    ceiling(diff(range(x))/h)
}

nclass.FD <- function(x)
{
    r <- as.vector(quantile(x, c(0.25, 0.75)))
    h <- 2 * (r[2] - r[1]) * length(x)^(-1/3)
    ceiling(diff(range(x))/h)
}


