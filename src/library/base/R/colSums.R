colSums <- function(x, na.rm = FALSE, dims = 1)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1 || dims > length(dn) - 1)
        stop("invalid 'dims'")
    n <- prod(dn[1:dims])
    dn <- dn[-(1:dims)]
    z <- if(is.complex(x))
        .Internal(colSums(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colSums(Im(x), n, prod(dn), na.rm))
    else .Internal(colSums(x, n, prod(dn), na.rm))
    if(length(dn) > 1) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-(1:dims)]
    } else names(z) <- dimnames(x)[[dims+1]]
    z
}

colMeans <- function(x, na.rm = FALSE, dims = 1)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1 || dims > length(dn) - 1)
        stop("invalid 'dims'")
    n <- prod(dn[1:dims])
    dn <- dn[-(1:dims)]
    z <- if(is.complex(x))
        .Internal(colMeans(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colMeans(Im(x), n, prod(dn), na.rm))
    else .Internal(colMeans(x, n, prod(dn), na.rm))
    if(length(dn) > 1) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-(1:dims)]
    } else names(z) <- dimnames(x)[[dims+1]]
    z
}

rowSums <- function(x, na.rm = FALSE, dims = 1)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1 || dims > length(dn) - 1)
        stop("invalid 'dims'")
    p <- prod(dn[-(1:dims)])
    dn <- dn[1:dims]
    z <- if(is.complex(x))
        .Internal(rowSums(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowSums(Im(x), prod(dn), p, na.rm))
    else .Internal(rowSums(x, prod(dn), p, na.rm))
    if(length(dn) > 1) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[1:dims]
    } else  names(z) <- dimnames(x)[[1]]
    z
}

rowMeans <- function(x, na.rm = FALSE, dims = 1)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1 || dims > length(dn) - 1)
        stop("invalid 'dims'")
    p <- prod(dn[-(1:dims)])
    dn <- dn[1:dims]
    z <- if(is.complex(x))
        .Internal(rowMeans(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowMeans(Im(x), prod(dn), p, na.rm))
    else .Internal(rowMeans(x, prod(dn), p, na.rm))
    if(length(dn) > 1) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[1:dims]
    } else  names(z) <- dimnames(x)[[1]]
    z
}
