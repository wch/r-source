solve.qr <- function(a, b, ...)
{
    if( !is.qr(a) )
	stop("this is the \"qr\" method for the generic function solve()")
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    return(qr.coef(a, b))
}

solve.default <-
    function(a, b, tol = ifelse(LINPACK, 1e-7, .Machine$double.eps),
	     LINPACK = FALSE, ...)
{
    if(is.complex(a) || (!missing(b) && is.complex(b))) {
	a <- as.matrix(a)
	if(missing(b)) {
	    b <- diag(1+0i, nrow(a))
	    colnames(b) <- rownames(a)
	} else if(!is.complex(b)) b[] <- as.complex(b)
	if(!is.complex(a)) a[] <- as.complex(a)
	return (if (is.matrix(b)) {
	    rownames(b) <- colnames(a)
	    .Call("La_zgesv", a, b, PACKAGE = "base")
	} else
	    drop(.Call("La_zgesv", a, as.matrix(b), PACKAGE = "base")))
    }
    if(is.qr(a)) {
	warning("solve.default called with a \"qr\" object: use 'qr.solve'")
	return(solve.qr(a, b, tol))
    }

    if(!LINPACK) {
	a <- as.matrix(a)
	if(missing(b)) {
	    b <- diag(1.0, nrow(a))
	    colnames(b) <- rownames(a)
	} else storage.mode(b) <- "double"
	storage.mode(a) <- "double"
	return (if (is.matrix(b)) {
	    rownames(b) <- colnames(a)
	    .Call("La_dgesv", a, b, tol, PACKAGE = "base")
	} else
	    drop(.Call("La_dgesv", a, as.matrix(b), tol, PACKAGE = "base")))
    }
    a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	## preserve dimnames
	b <- diag(1, nc)
	colnames(b) <- rownames(a$qr)
    }
    qr.coef(a, b)
}

solve <- function(a, b, ...) UseMethod("solve")

qr.solve <- function(a, b, tol = 1e-7)
{
    if( !is.qr(a) )
	a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix 'a' in solve")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    return(qr.coef(a, b))
}

