cor <- function (x, y=NULL, use="all.obs")
{
    na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
    if(is.data.frame(x)) x <- as.matrix(x)
    if(is.data.frame(y)) y <- as.matrix(y)
    if(!is.matrix(x) && is.null(y))
        stop("supply both x and y or a matrix-like x")
    .Internal(cor(x, y, na.method))
}

cov2cor <- function(V)
{
    ## Purpose: Covariance matrix |--> Correlation matrix -- efficiently
    ## ----------------------------------------------------------------------
    ## Arguments: V: a covariance matrix (i.e. symmetric and positive definite)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 12 Jun 2003, 11:50
    p <- (d <- dim(V))[1]
    if(!is.numeric(V) || length(d) != 2 || p != d[2])
        stop("`V' is not a square numeric matrix")
    Is <- sqrt(1/diag(V)) # diag( 1/sigma_i )
    if(any(!is.finite(Is)))
        warning("diagonal has non-finite entries")
    r <- V # keep dimnames
    r[] <- Is * V * rep(Is, each = p)
    ##  == D %*% V %*% D  where D = diag(Is)
    r[cbind(1:p,1:p)] <- 1 # exact in diagonal
    r
}
