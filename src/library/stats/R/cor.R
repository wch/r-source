#### cor() , cov() and var() : Based on the same C code

## cor() and cov() only differ by one single letter :
cor <-
function(x, y=NULL, use="all.obs", method = c("pearson", "kendall", "spearman"))
{
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
    method <- match.arg(method)
    if(is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    if(is.data.frame(x)) x <- as.matrix(x)
    else {
	stopifnot(is.atomic(x))
	if(!is.matrix(x)) {
	    if(is.null(y)) stop("supply both 'x' and 'y' or a matrix-like 'x'")
	    x <- as.vector(x)
	}
    }
    if(method == "pearson")
        .Internal(cor(x, y, na.method, method == "kendall"))
    else if (na.method != 3) {
	## Rank transform
	Rank <- function(u)
	    if(is.matrix(u)) apply(u, 2, rank, na.last="keep")
	    else rank(u, na.last="keep")

        if (na.method == 2){ # complete.obs
            ok <- complete.cases(x,y)
            x <- if (is.matrix(x)) x[ok,] else x[ok]
            if(!is.null(y)) y <- if(is.matrix(y)) y[ok,] else y[ok]
        }

	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
        .Internal(cor(x, y, na.method, method == "kendall"))
    }
    else { # rank correlations and pairwise complete; the hard case
         ## Based on contribution from Shigenobu Aoki.
         ## matrix
         if (is.null(y)) {
             ncy <- ncx <- ncol(x)
             r <- matrix(0, nrow=ncx, ncol=ncy)
             for (i in 2:ncx) {
                 for (j in 1:(i-1)) {
                     x2 <- x[,i]
                     y2 <- x[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     r[i, j] <- .Internal(cor(x2, y2, na.method, method == "kendall"))
                 }
             }
             r <- r+t(r)
             diag(r) <- 1
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(x)
             r
         }
         ## matrix x matrix
         else {
	     if (!is.matrix(x)) x <- matrix(x, ncol=1)
	     if (!is.matrix(y)) y <- matrix(y, ncol=1)
             ncx <- ncol(x)
             ncy <- ncol(y)
             r <- matrix(0, nrow=ncx, ncol=ncy)
             for (i in 1:ncx) {
                 for (j in 1:ncy) {
                     x2 <- x[,i]
                     y2 <- y[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     r[i, j] <- .Internal(cor(x2, y2, na.method, method == "kendall"))
                 }
             }
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(y)
             r
         }
     }
}

cov <-
function(x, y=NULL, use="all.obs", method = c("pearson", "kendall", "spearman"))
{
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
    method <- match.arg(method)
    if(is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    if(is.data.frame(x)) x <- as.matrix(x)
    else {
	stopifnot(is.atomic(x))
	if(!is.matrix(x)) {
	    if(is.null(y)) stop("supply both 'x' and 'y' or a matrix-like 'x'")
	    x <- as.vector(x)
	}
    }
    if(method == "pearson")
        .Internal(cov(x, y, na.method, method == "kendall"))
    else if (na.method != 3) {
	## Rank transform
	Rank <- function(u)
	    if(is.matrix(u)) apply(u, 2, rank, na.last="keep")
	    else rank(u, na.last="keep")

        if (na.method == 2){ # complete.obs
            ok <- complete.cases(x,y)
            x <- if (is.matrix(x)) x[ok,] else x[ok]
            if(!is.null(y)) y <- if(is.matrix(y)) y[ok,] else y[ok]
        }

	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
        .Internal(cov(x, y, na.method, method == "kendall"))
    }
    else
        stop("cannot handle 'pairwise.complete.obs'")
}

var <- function(x, y = NULL, na.rm = FALSE, use) {
    if(missing(use))
	use <- if(na.rm) "complete.obs" else "all.obs"
    na.method <- pmatch(use, c("all.obs", "complete.obs",
			       "pairwise.complete.obs"))
    if (is.data.frame(x)) x <- as.matrix(x) else stopifnot(is.atomic(x))
    if (is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    .Internal(cov(x, y, na.method, FALSE))
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
	stop("'V' is not a square numeric matrix")
    Is <- sqrt(1/diag(V)) # diag( 1/sigma_i )
    if(any(!is.finite(Is)))
	warning("diagonal has non-finite entries")
    r <- V # keep dimnames
    r[] <- Is * V * rep(Is, each = p)
    ##	== D %*% V %*% D  where D = diag(Is)
    r[cbind(1:p,1:p)] <- 1 # exact in diagonal
    r
}
