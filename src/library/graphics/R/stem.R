stem <- function(x, scale = 1, width = 80, atom = 0.00000001) {
    if (!is.numeric(x) )
	stop("x must be numeric")
    x <- x[!is.na(x)]
    if (length(x)==0) stop("no non-missing values")
    if (scale <= 0) stop("scale must be positive")# unlike S
    .C("stemleaf", as.double(x), length(x),
       as.double(scale), as.integer(width), as.double(atom), PACKAGE="base")
    invisible(NULL)
}
