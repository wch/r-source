chol <- function(x)
{
    if(!is.numeric(x))
	stop("non-numeric argument to chol")

    if(is.matrix(x)) {
	if(nrow(x) != ncol(x))
	    stop("non-square matrix in chol")
	n <- nrow(x)
    }
    else {
	if(length(x) != 1)
	    stop("non-matrix argument to chol")
	n <- as.integer(1)
    }

    if(!is.double(x)) storage.mode(x) <- "double"

    z <- .Fortran("chol",
		  x=x,
		  n,
		  n,
		  v=matrix(0, nr=n, nc=n),
		  info=integer(1),
		  DUP=FALSE)
    if(z$info)
	stop("singular matrix in chol")
    z$v
}

chol2inv <- function(x, size=ncol(x))
{
    if(!is.numeric(x))
	stop("non-numeric argument to chol2inv")
    if(is.matrix(x)) {
	nr <- nrow(x)
	nc <- ncol(x)
    }
    else {
	nr <- length(x)
	nc <- as.integer(1)
    }
    size <- as.integer(size)
    if(size <= 0 || size > nr || size > nc)
	stop("invalid size argument in chol2inv")
    if(!is.double(x)) storage.mode(x) <- "double"
    z <- .Fortran("ch2inv",
		  x=x,
		  nr,
		  size,
		  v=matrix(0, nr=size, nc=size),
		  info=integer(1),
		  DUP=FALSE)
    if(z$info)
	stop("singular matrix in chol2inv")
    z$v
}
