pretty <- function(x, n=5, shrink.sml = 2^-3) {
	if(!is.numeric(x))
		stop("x must be numeric")
        if(length(x)==0)
          	return(x)
	if(is.na(n <- n[1]) || n < 1)
		stop("invalid n value")
	z <- .C("pretty",l=min(x),u=max(x),n=as.integer(n),
                shrink = as.real(shrink.sml))
	seq(z$l,z$u,length=z$n+1)
}
