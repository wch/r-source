cmdscale <- function (d, k = 2, eig = FALSE) {
    if (any(is.na(d)))
	stop("NA values not allowed in d")
    if (is.null(n <- attr(d, "Size"))) {
	x <- as.matrix(d^2)
	if ((n <- nrow(x)) != ncol(x))
	    stop("Distances must be result of dist or a square matrix")
    }
    else {
	x <- matrix(0, n, n)
	x[row(x) > col(x)] <- d^2
	x <- x + t(x)
    }
    storage.mode(x) <- "double"
    Tmat <- -0.5 * .C("dblcen", x, as.integer(n), PACKAGE="mva")[[1]]
    e <- eigen(Tmat, symmetric = TRUE)
    ev <- e$values[1:k]
    points <- e$vectors[, 1:k] %*% diag(sqrt(ev))
    dimnames(points) <- list(dimnames(d)[[1]], NULL)
    if (eig) list(points = points, eig = ev)
    else points
}
