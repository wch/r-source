D <- function(expr, name) .Internal(D(expr, name))

deriv <- function(expr, ...) UseMethod("deriv")

deriv.formula <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = FALSE, ...)
{
    if((le <- length(expr)) > 1)
	.Internal(deriv.default(expr[[le]], namevec, function.arg, tag, hessian))
    else stop("invalid formula in deriv")
}

deriv.default <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = FALSE, ...)
    .Internal(deriv.default(expr, namevec, function.arg, tag, hessian))

deriv3 <- function(expr, ...) UseMethod("deriv3")

deriv3.formula <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = TRUE, ...)
{
    if((le <- length(expr)) > 1)
	.Internal(deriv.default(expr[[le]], namevec, function.arg, tag, hessian))
    else stop("invalid formula in deriv")
}

deriv3.default <- function(expr, namevec, function.arg=NULL, tag=".expr",
                          hessian = TRUE, ...)
    .Internal(deriv.default(expr, namevec, function.arg, tag, hessian))

