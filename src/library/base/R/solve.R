qr.solve <- function(a, b, tol = 1e-7)
{
	if( !is.qr(a) )
		a <- qr(a, tol = tol)
	nc <- ncol(a$qr)
	if( a$rank != nc )
		stop("singular matrix `a' in solve")
	if( missing(b) ) {
		if( nc != nrow(a$qr) )
			stop("only square matrices can be inverted")
		b<-diag(1,nc)
	}
	b<-as.matrix(b)
	return(qr.coef(a,b))
}

solve <- function(a, b, ...) UseMethod("solve")
solve.default <- qr.solve
solve.qr <- qr.solve
