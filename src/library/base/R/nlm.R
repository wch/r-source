nlm <- function(f, p, hessian=FALSE, typsize=rep(1,length(p)),
		fscale=1, print.level=0, ndigit=12, gradtol=1e-6,
		stepmax=max(1000 * sqrt(sum((p/typsize)^2)), 1000),
		steptol=1e-6, iterlim=100)
{

    print.level <- as.integer(print.level)
    if(print.level < 0 || print.level > 2)
	stop("`print.level' must be in {0,1,2}")
    msg <- c(9,1,17)[1+print.level]
    .Internal(nlm(f, p, hessian, typsize, fscale, msg, ndigit, gradtol,
		  stepmax, steptol, iterlim))
}

optimize <- function(f, interval, lower=min(interval), upper=max(interval),
		     maximum=FALSE, tol=.Machine$double.eps^0.25, ...)
{
    if(maximum) {
	val <- .Internal(fmin(function(arg) -f(arg, ...), lower, upper, tol))
	list(maximum=val, objective= f(val, ...))
    } else {
	val <- .Internal(fmin(function(arg) f(arg, ...), lower, upper, tol))
	list(minimum=val, objective= f(val, ...))
    }
}

##nice to the English
optimise <- .Alias(optimize)

uniroot <- function(f, interval, lower=min(interval), upper=max(interval),
		    tol=.Machine$double.eps^0.25, maxiter = 1000, ...)
{
    if(!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
		   stop("lower < upper  is not fulfilled")
    if(f(lower, ...)*f(upper, ...) >= 0)
	stop("f() values at end points not of opposite sign")
    val <- .Internal(zeroin(function(arg) f(arg, ...), lower, upper, tol,
			    as.integer(maxiter)))
    if((iter <- as.integer(val[2])) < 0) {
	warning(paste("_NOT_ converged in ",maxiter,"iterations."))
        iter <- -iter
    }
    list(root=val[1], f.root=f(val[1], ...),
         iter=iter, estim.prec= val[3])
}

deriv <- function(x, ...) UseMethod("deriv")

deriv.formula <- function(expr, namevec, function.arg=NULL, tag=".expr") {
    if((le <- length(expr)) > 1)
	.Internal(deriv.default(expr[[le]], namevec, function.arg, tag))
    else stop("invalid formula in deriv")
}

deriv.default <- function(expr, namevec, function.arg=NULL, tag=".expr")
    .Internal(deriv.default(expr, namevec, function.arg, tag))
