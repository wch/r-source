### From Doug Bates' 20 Apr 1999 post to R-devel;
### "method" idea from J.K.Lindsey's rmutil
det <- function(x, method = c("qr", "eigenvalues"), tol = 1e-07)
{
    if(!is.matrix(x) || (n <- ncol(x)) != nrow(x))
	stop("x must be a square matrix")
    method <- match.arg(method) # ensures one from above
    if(method == "qr") {
        qx <- qr(x, tol = tol)
        if(qx$rank < n) return(0);
        x <- prod(diag(qx$qr))
        if(n %% 2 == 1) x else -x
    } else ## method == "eigenvalues"
	Re(prod(eigen(x, only.values=TRUE)$values))
}

## S-plus' Matrix pkg has arg. "logarithm = TRUE" and returns list
##        (which is necessary for keeping the sign when taking log ..)
## S-plus v 6.x has incorporated the Matrix pkg det as determinant

ndet = function(x, ...)
{
    z = determinant(x, logarithm = TRUE, ...)
    c(z$sign * exp(z$modulus))
}

determinant = function(x, logarithm = TRUE, ...) UseMethod("determinant")

determinant.matrix = function(x, logarithm = TRUE, ...)
{
    if ((n <- ncol(x)) != nrow(x))
        stop("x must be a square matrix")
    if (n < 1)
        return(list(modulus = double(0), sign = as.integer(1),
                    logarithm = logarithm))
    if (is.complex(x))
        stop("determinant not currently defined for complex matrices")
    storage.mode(x) = "double"
    .Call("det_ge_real", x, logarithm)
#    dc = .Fortran("dgetrf", n, n, a=x, n, ipiv=integer(n), info=integer(1))
#    if (dc$info) stop(paste("dgetrf returned error code", dc$info))
#    modulus = sum(log(abs(da <- diag(dc$a))))
#    return(list(modulus = ifelse(as.logical(logarithm)[1], modulus, exp(modulus)),
#                sign = ifelse(sum(da < 0, dc$ipiv != 1:n) %% 2, -1, 1)))
}


