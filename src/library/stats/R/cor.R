#  File src/library/stats/R/cor.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

#### cor() , cov() and var() : Based on the same C code

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
    else if (na.method != 3L) {
	## Rank transform
	Rank <- function(u) {
            ## take care not to drop dims on a 0-row matrix
            if(length(u) == 0L) u else
            if(is.matrix(u)) apply(u, 2L, rank, na.last="keep")
            else rank(u, na.last="keep")
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
             ## 2.6.0 assumed the diagonal was 1, but not so for all NAs,
             ## nor single non-NA pairs.
             for (i in seq.int(1L, length.out = ncx)) {
                 for (j in seq_len(i)) {
                     x2 <- x[,i]
                     y2 <- x[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     ## we've removed all NAs
                     r[i, j] <- if(any(ok)) .Internal(cor(x2, y2, 1L, method == "kendall")) else NA
                 }
             }
             r <- r + t(r) - diag(diag(r))
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(x)
             r
         }
         ## matrix x matrix
         else {
	     if (!is.matrix(x)) x <- matrix(x, ncol=1L)
	     if (!is.matrix(y)) y <- matrix(y, ncol=1L)
             ncx <- ncol(x)
             ncy <- ncol(y)
             r <- matrix(0, nrow=ncx, ncol=ncy)
             for (i in 1L:ncx) {
                 for (j in 1L:ncy) {
                     x2 <- x[,i]
                     y2 <- y[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     r[i, j] <- if(any(ok)) .Internal(cor(x2, y2, 1L, method == "kendall")) else NA
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
    else if (na.method != 3L) {
	## Rank transform
	Rank <- function(u) {
            if(length(u) == 0) u else
            if(is.matrix(u)) apply(u, 2, rank, na.last="keep")
            else rank(u, na.last="keep")
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
    if(!is.numeric(V) || length(d) != 2L || p != d[2L])
	stop("'V' is not a square numeric matrix")
    Is <- sqrt(1/diag(V)) # diag( 1/sigma_i )
    if(any(!is.finite(Is)))
	warning("diag(.) had 0 or NA entries; non-finite result is doubtful")
    r <- V # keep dimnames
    r[] <- Is * V * rep(Is, each = p)
    ##	== D %*% V %*% D  where D = diag(Is)
    r[cbind(1L:p,1L:p)] <- 1 # exact in diagonal
    r
}
