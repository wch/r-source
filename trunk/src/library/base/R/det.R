## det now uses Lapack and an LU decomposition.  The method argument is
##     no longer used.
## S-plus' Matrix pkg has arg. "logarithm = TRUE" and returns list
##        (which is necessary for keeping the sign when taking log ..)
## S-plus v 6.x has incorporated the Matrix pkg det as determinant

det = function(x, ...)
{
    z = determinant(x, logarithm = TRUE, ...)
    c(z$sign * exp(z$modulus))
}

determinant = function(x, logarithm = TRUE, ...) UseMethod("determinant")

determinant.matrix = function(x, logarithm = TRUE, ...)
{
    if ((n <- ncol(x)) != nrow(x))
        stop("'x' must be a square matrix")
    if (n < 1)
        return(list(modulus = double(0), sign = as.integer(1),
                    logarithm = logarithm))
    if (is.complex(x))
        stop("determinant not currently defined for complex matrices")
    storage.mode(x) = "double"
    .Call("det_ge_real", x, logarithm, PACKAGE = "base")
}
