scale <- function(x, ..., scale = TRUE) UseMethod("scale")

scale.default <- function(x, center = TRUE, scale = TRUE)
{
    x <- as.matrix(x)
    nc <- ncol(x)
    if (is.logical(center)) {
	if (center)
	    x <- sweep(x, 2, apply(x, 2, mean, na.rm=TRUE))
    }
    else if (is.numeric(center) && (length(center) == nc))
	x <- sweep(x, 2, center)
    else
	stop("Length of center must equal the number of columns of x")
    if (is.logical(scale)) {
	if (scale) {
	    f <- function(v) {
		nas <- is.na(v)
		if(any(is.na(nas)))
		    v <- v[!is.na(nas)]
		sqrt(sum(v^2) / max(1, length(v) - 1))
	    }
	    x <- sweep(x, 2, apply(x, 2, f), "/")
	}
    }
    else if (is.numeric(scale) && length(scale) == nc)
	x <- sweep(x, 2, scale, "/")
    else
	stop("Length of scale must equal the number of columns of x")
    x
}
