rep <- function(x, times, length.out)
{
    if (length(x) == 0)
	return(x)
    if (missing(times))
	times <- ceiling(length.out/length(x))
    r <- .Internal(rep(x,times))
    if(!is.null(nm <- names(x))) names(r) <- .Internal(rep(nm, times))
    if (!missing(length.out))
	return(r[if(length.out>0) 1:length.out else integer(0)])
    return(r)
}
