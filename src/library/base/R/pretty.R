pretty <- function(x, n=5, shrink.sml = 2^-3) {
	if(!is.numeric(x))
		stop("x must be numeric")
        if(length(x)==0)
          	return(x)
	if(is.na(n <- n[1]) || n < 1)
		stop("invalid n value")
	if(!is.numeric(shrink.sml) || shrink.sml <= 1e-8)
		stop("argument `shrink.sml' must be numeric > 1e-8")
	z <- .C("pretty",l=as.real(min(x)),u=as.real(max(x)),n=as.integer(n),
                shrink = as.real(shrink.sml))
	seq(z$l,z$u,length=z$n+1)
}
