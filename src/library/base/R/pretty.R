pretty <- function(x, n=5, min.n= n %/% 3, shrink.sml = 0.75,
                   high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
                   eps.correct = 0)
{
    x <- as.numeric(x)
    if(length(x)==0)
	return(x)
    x <- x[is.finite(x)]
    if(is.na(n <- as.integer(n[1])) || n < 0)# n=0 !!
	stop("invalid 'n' value")
    if(!is.numeric(shrink.sml) || shrink.sml <= 0)
	stop("'shrink.sml' must be numeric > 0")
    if((min.n <- as.integer(min.n)) < 0 || min.n > n)
	stop("'min.n' must be non-negative integer <= n")
    if(!is.numeric(high.u.bias) || high.u.bias < 0)
	stop("'high.u.bias' must be non-negative numeric")
    if(!is.numeric(u5.bias) || u5.bias < 0)
	stop("'u5.bias' must be non-negative numeric")
    if((eps.correct <- as.integer(eps.correct)) < 0 || eps.correct > 2)
	stop("'eps.correct' must be 0, 1, or 2")
    z <- .C("R_pretty", l=as.double(min(x)), u=as.double(max(x)),
            n = n,
            min.n,
	    shrink = as.double(shrink.sml),
            high.u.fact = as.double(c(high.u.bias, u5.bias)),
            eps.correct,
            DUP = FALSE, PACKAGE = "base")
    s <- seq.int(z$l, z$u, length.out = z$n+1)
    if(!eps.correct && z$n) { # maybe zap smalls from seq() rounding errors
        ## better than zapsmall(s, digits = 14) :
        delta <- diff(range(z$l, z$u)) / z$n
        if(any(small <- abs(s) < 1e-14 * delta))
            s[small] <- 0
    }
    s
}
