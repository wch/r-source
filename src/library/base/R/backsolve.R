backsolve <- function(r, x, k=ncol(r))
{
	r <- as.matrix(r)# nr  x  k
	x <- as.matrix(x)#  k  x  nb
	if(k <= 0 || nrow(x) != k) stop("invalid parameters in backsolve")
        nb <- ncol(x)
	z <- .C("bakslv",
                t  = as.double(r),
                ldt= nrow(r),
                n  = k,
                b  = as.double(x),
                ldb= k,
                nb = nb,
                x  = matrix(0, k, nb),
                job= as.integer(1),
                info= integer(1),
                DUP= FALSE)
	if(z$info != 0) stop("singular matrix in backsolve")
	z$x
}
