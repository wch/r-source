chol <- function(x, pivot = FALSE, LINPACK = pivot)
{
    if (is.complex(x))
        stop("complex matrices not permitted at present")
    else if(!is.numeric(x))
	stop("non-numeric argument to 'chol'")

    if(is.matrix(x)) {
	if(nrow(x) != ncol(x))
	    stop("non-square matrix in 'chol'")
	n <- nrow(x)
    } else {
	if(length(x) != 1)
	    stop("non-matrix argument to 'chol'")
	n <- as.integer(1)
    }
    if(!pivot && !LINPACK)
        return(.Call("La_chol", as.matrix(x), PACKAGE = "base"))

    if(!is.double(x)) storage.mode(x) <- "double"

    if(pivot) {
        xx <- x
        xx[lower.tri(xx)] <- 0
        z <- .Fortran("dchdc",
                      x = xx,
                      n,
                      n,
                      double(n),
                      piv = as.integer(rep.int(0, n)),
                      as.integer(pivot),
                      rank = integer(1),
                      DUP = FALSE, PACKAGE = "base")
        if (!pivot && z$rank < n)
            stop("matrix not positive definite")
        robj <- z$x
        if (pivot) {
            attr(robj, "pivot") <- z$piv
            attr(robj, "rank") <- z$rank
            if(!is.null(cn <- colnames(x)))
                colnames(robj) <- cn[z$piv]
        }
        robj
    } else {
        z <- .Fortran("chol",
                      x = x,
                      n,
                      n,
                      v = matrix(0, nr=n, nc=n),
                      info = integer(1),
                      DUP = FALSE, PACKAGE = "base")
        if(z$info)
            stop("non-positive definite matrix in 'chol'")
        z$v
    }
}

chol2inv <- function(x, size=NCOL(x), LINPACK=FALSE)
{
    if(!is.numeric(x))
	stop("non-numeric argument to 'chol2inv'")
    if(!LINPACK) return(La.chol2inv(x, size))

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
	stop("invalid 'size' argument in 'chol2inv'")
    if(!is.double(x)) storage.mode(x) <- "double"
    z <- .Fortran("ch2inv",
		  x=x,
		  nr,
		  size,
		  v=matrix(0, nr=size, nc=size),
		  info=integer(1),
		  DUP=FALSE, PACKAGE="base")
    if(z$info)
	stop("singular matrix in 'chol2inv'")
    z$v
}
