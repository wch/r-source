pretty <- function(x, n=5, min.n= n %/% 3, shrink.sml = 0.75,
                   high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
                   eps.correct = 0)
{
    if(!is.numeric(x))
	stop("x must be numeric")
    if(length(x)==0)
	return(x)
    if(is.na(n <- as.integer(n[1])) || n < 0)# n=0 !!
	stop("invalid n value")
    if(!is.numeric(shrink.sml) || shrink.sml <= 0)
	stop("argument `shrink.sml' must be numeric > 0")
    if((min.n <- as.integer(min.n)) < 0 || min.n > n)
	stop("argument `min.n' must be non-negative integer <= n")
    if(!is.numeric(high.u.bias) || high.u.bias < 0)
	stop("argument `high.u.bias' must be non-negative numeric")
    if(!is.numeric(u5.bias) || u5.bias < 0)
	stop("argument `u5.bias' must be non-negative numeric")
    if((eps.correct <- as.integer(eps.correct)) < 0 || eps.correct > 2)
	stop("argument `eps.correct' must be 0, 1, or 2")
    z <- .C("pretty", l=as.double(min(x)), u=as.double(max(x)),
            n = n,
            min.n,
	    shrink = as.double(shrink.sml),
            high.u.fact = as.double(c(high.u.bias, u5.bias)),
            eps.correct,
            DUP = FALSE, PACKAGE = "base")
    seq(z$l, z$u, length=z$n+1)
}
