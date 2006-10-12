mahalanobis <- function(x, center, cov, inverted=FALSE, ...)
{
    x <- if(is.vector(x)) matrix(x, ncol=length(x)) else as.matrix(x)
    ## save speed in customary case:
    ## if(any(center != 0))
    x <- sweep(x, 2, center)# = (x - center)

    ## The following would be considerably faster for  small nrow(x) and
    ## slower otherwise; probably always faster if the two t(.) weren't needed:
    ##
    ##	retval <- apply(x * if(inverted) x%*%cov
    ##	                    else    t(solve(cov,t(x), tol=tol.inv)),
    ##			1, sum)
    if(!inverted)
	cov <- solve(cov, ...)
    retval <- rowSums((x%*%cov) * x)
    names(retval) <- rownames(x)
    retval
}
