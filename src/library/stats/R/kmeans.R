kmeans <- function(x, centers, iter.max = 10)
{
    x <- as.matrix(x)
    m <- nrow(x)
    if(missing(centers))
	stop("centers must be a number or a matrix")
    if(length(centers) == 1) {
	k <- centers
	if(m < k)
	    stop("more cluster centers than data points.")
	centers <- x[sample(1:m, k), , drop=FALSE]
    } else {
	centers <- as.matrix(centers)
	k <- nrow(centers)
    }
    if(iter.max < 1) stop("iter.max must be positive")
    if(m < k)
	stop("more cluster centers than data points")
    if(ncol(x) != ncol(centers))
	stop("must have same number of columns in x and centers")
    Z <- .Fortran("kmns",
		  as.double(x),
		  as.integer(m),
		  as.integer(ncol(x)),
		  centers = as.double(centers),
		  as.integer(k),
		  c1 = integer(m),
		  integer(m),
		  nc =integer(k),
		  double(k),
		  double(k),
		  integer(k),
		  double(m),
		  integer(k),
		  integer(k),
		  as.integer(iter.max),
		  wss = double(k),
		  ifault = as.integer(0), PACKAGE="stats")
    switch(Z$ifault,
	   stop("empty cluster: try a better set of initial centers",
                call.=FALSE),
	   warning("did not converge in iter.max iterations", call.=FALSE),
	   stop("number of cluster centres must lie between 1 and nrow(x)",
                call.=FALSE)
	   )
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1:k, dimnames(x)[[2]])
    out <- list(cluster = Z$c1, centers = centers, withinss = Z$wss,
                size = Z$nc)
    class(out) <- "kmeans"
    out
}

