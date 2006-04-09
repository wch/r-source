scale <- function(x, center = TRUE, scale = TRUE) UseMethod("scale")

scale.default <- function(x, center = TRUE, scale = TRUE)
{
    x <- as.matrix(x)
    nc <- ncol(x)
    if (is.logical(center)) {
	if (center) {
            center <- colMeans(x, na.rm=TRUE)
	    x <- sweep(x, 2, center)
        }
    }
    else if (is.numeric(center) && (length(center) == nc))
	x <- sweep(x, 2, center)
    else
	stop("length of 'center' must equal the number of columns of 'x'")
    if (is.logical(scale)) {
	if (scale) {
	    f <- function(v) {
		v <- v[!is.na(v)]
		sqrt(sum(v^2) / max(1, length(v) - 1))
	    }
            scale <- apply(x, 2, f)
	    x <- sweep(x, 2, scale, "/")
	}
    }
    else if (is.numeric(scale) && length(scale) == nc)
	x <- sweep(x, 2, scale, "/")
    else
	stop("length of 'scale' must equal the number of columns of 'x'")
    if(is.numeric(center)) attr(x, "scaled:center") <- center
    if(is.numeric(scale)) attr(x, "scaled:scale") <- scale
    x
}
