backsolve <-
function(r, x, k=ncol(r))
{
	r <- as.matrix(r)
	x <- as.matrix(x)
	if(k <= 0 || nrow(x) != k) stop("invalid parameters in backsolve")
	z <- .Fortran("bkslv",
		as.double(r),
		nrow(r),
		as.integer(k),
		as.double(x),
		as.integer(k),
		y=matrix(0, k, ncol(x)),
		as.integer(1),
		info=integer(1),
		DUP=FALSE)
	if(z$info != 0) stop("singular matrix in backsolve")
	z$y
}
