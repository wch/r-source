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
	    if(is.null(y)) stop("supply both x and y or a matrix-like x")
	    x <- as.vector(x)
	}
    }
    if(method != "pearson") {
	## Rank transform
	Rank <- function(u)
	    if(is.matrix(u)) apply(u, 2, rank, na.last="keep")
	    else rank(u, na.last="keep")
	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
    }
    .Internal(cor(x, y, na.method, method == "kendall"))
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
	    if(is.null(y)) stop("supply both x and y or a matrix-like x")
	    x <- as.vector(x)
	}
    }
    if(method != "pearson") {
	## Rank transform
	Rank <- function(u)
	    if(is.matrix(u)) apply(u, 2, rank, na.last="keep")
	    else rank(u, na.last="keep")
	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
    }
    .Internal(cov(x, y, na.method, method == "kendall"))
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
