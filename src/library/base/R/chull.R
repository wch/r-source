chull <- function(x, y=NULL)
{
    X <- xy.coords(x, y, recycle = TRUE)
    x <- cbind(X$x, X$y)
    n <- nrow(x)
    if(n == 0) return(integer(0))
    z <- .C("R_chull",
	    n=as.integer(n),
	    as.double(x),
	    as.integer(n),
	    as.integer(1:n),
	    integer(n),
	    integer(n),
	    ih=integer(n),
	    nh=integer(1),
	    il=integer(n),
	    PACKAGE="base")
    rev(z$ih[1:z$nh])
}
