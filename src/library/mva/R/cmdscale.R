cmdscale <- function (d, k = 2, eig = FALSE, x.ret = FALSE) {
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
    ## doubly center x in-place  -- DUP needs change in ../src/init.c -- "FIXME" naokfind
    ## .C("dblcen", x, as.integer(n), DUP = FALSE, PACKAGE="mva")
    x <- .C("dblcen", x=x, as.integer(n), PACKAGE="mva")$x
    e <- La.eigen(-x/2, symmetric = TRUE)
    ev <- e$values[1:k]
    points <- e$vectors[, 1:k] %*% diag(sqrt(ev))
    rn <- if(is.matrix(d)) rownames(d) else names(d)
    dimnames(points) <- list(rn, NULL)
    if (eig || x.ret)
        list(points = points,
             eig = if(eig) ev, x = if(x.ret) x)
    else points
}
