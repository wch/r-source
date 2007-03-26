nlm <- function(f, p, hessian=FALSE, typsize=rep(1,length(p)),
		fscale=1, print.level=0, ndigit=12, gradtol=1e-6,
		stepmax=max(1000 * sqrt(sum((p/typsize)^2)), 1000),
		steptol=1e-6, iterlim=100, check.analyticals=TRUE, ...)
{

    print.level <- as.integer(print.level)
    if(print.level < 0 || print.level > 2)
	stop("'print.level' must be in {0,1,2}")
    ## msg is collection of bits, i.e., sum of 2^k (k = 0,..,4):
    msg <- (1 + c(8,0,16))[1+print.level]
    if(!check.analyticals) msg <- msg + (2 + 4)
    .Internal(nlm(function(x) f(x, ...), p, hessian, typsize, fscale,
                  msg, ndigit, gradtol, stepmax, steptol, iterlim))
}

optimize <- function(f, interval, lower=min(interval), upper=max(interval),
		     maximum=FALSE, tol=.Machine$double.eps^0.25, ...)
{
    if(maximum) {
	val <- .Internal(fmin(function(arg) -f(arg, ...), lower, upper, tol))
	list(maximum = val, objective= f(val, ...))
    } else {
	val <- .Internal(fmin(function(arg) f(arg, ...), lower, upper, tol))
	list(minimum = val, objective= f(val, ...))
    }
}

##nice to the English (or rather the Scots)
optimise <- optimize

uniroot <- function(f, interval, lower = min(interval), upper = max(interval),
		    tol = .Machine$double.eps^0.25, maxiter = 1000, ...)
{
    if(!missing(interval) && length(interval) != 2)
        stop("'interval' must be a vector of length 2")
    if(!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
        stop("lower < upper  is not fulfilled")
    if(f(lower, ...) * f(upper, ...) > 0)
	stop("f() values at end points not of opposite sign")
    val <- .Internal(zeroin(function(arg) f(arg, ...), lower, upper, tol,
			    as.integer(maxiter)))
    iter <- as.integer(val[2])
    if(iter < 0) {
	warning("_NOT_ converged in ", maxiter, " iterations")
        iter <- maxiter
    }
    list(root = val[1], f.root = f(val[1], ...),
         iter = iter, estim.prec = val[3])
}
