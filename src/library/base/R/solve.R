solve.qr <- function(a, b, tol = 1e-7, ...)
{
    if( !is.qr(a) )
	a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix `a' in solve")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    return(qr.coef(a, b))
}

solve.default <- function(a, b, tol = 1e-7, ...)
{
    if(is.complex(a) || (!missing(b) && is.complex(b))) {
        ## call overwrites a and b, so need to force copies
        A <- a
        if(missing(b)) B <- diag(1+0i, nrow(a)) else B <- b
        if(!is.complex(A)) A[] <- as.complex(A)
        if(!is.complex(B)) B[] <- as.complex(B)
        return (if (is.matrix(B))
	    .Call("La_zgesv", A, B, PACKAGE = "base")
	else
	    drop(.Call("La_zgesv", A, as.matrix(B), PACKAGE = "base")))
    }
    if( !is.qr(a) )
	a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix `a' in solve")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    qr.coef(a, b)
}

solve <- function(a, b, ...) UseMethod("solve")
qr.solve <- function(a, b, tol = 1e-7) solve.qr(a, b, tol)
