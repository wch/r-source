### Unimplemented Idea {for amount = NULL ?}
### Really "optimal" (e.g. for rug()), use a non-constant amount,
### e.g. use "d" = diff(xx)  BEFORE  taking min()...

jitter <- function(x, factor = 1, amount=NULL)
{
    if(length(x) == 0)
	return(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    z <- diff(r <- range(x[is.finite(x)]))
    if(z == 0) z <- abs(r[1])
    if(z == 0) z <- 1

    if(is.null(amount)) {		# default: Find 'necessary' amount
	d <- diff(xx <- unique(sort.int(round(x, 3 - floor(log10(z))))))
	d <- if(length(d)) min(d) else if(xx!=0) xx/10 else z/10
	amount <- factor/5 * abs(d)
    } else if(amount == 0)		# only then: S compatibility
	amount <- factor * (z/50)

    x + stats::runif(length(x),  - amount, amount)
}
