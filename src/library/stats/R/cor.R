#  File src/library/stats/R/cor.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

cor <- function(x, y = NULL, use = "everything",
                method = c("pearson", "kendall", "spearman"))
{
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
		      "everything", "na.or.complete"))
    if(is.na(na.method)) stop("invalid 'use' argument")
    method <- match.arg(method)
    if(is.data.frame(y)) y <- as.matrix(y)
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.matrix(x) && is.null(y))
        stop("supply both 'x' and 'y' or a matrix-like 'x'")
    ## non-atomic x should not be 'numeric', but in case a method messes up
    ## allow logicals for back-compatibility (package mice).
    if(!(is.numeric(x) || is.logical(x))) stop("'x' must be numeric")
    stopifnot(is.atomic(x))
    if(!is.null(y)) {
        if(!(is.numeric(y) || is.logical(y))) stop("'y' must be numeric")
        stopifnot(is.atomic(y))
    }

    ## Rank transform
    Rank <- function(u) {
        ## take care not to drop dims on a 0- or 1-row matrix
        if(length(u) == 0L) u
        else if(is.matrix(u)) {
            if(nrow(u) > 1L) apply(u, 2L, rank, na.last="keep") else row(u)
        } else rank(u, na.last="keep")
    }
    if(method == "pearson")
        .Call(C_cor, x, y, na.method, FALSE)
    else if (na.method %in% c(2L, 5L)) { ## "complete.obs" / "na.or.complete"
        if (is.null(y)) {
            .Call(C_cor, Rank(na.omit(x)), NULL, na.method,
                  method == "kendall")
        } else {
            nas <- attr(na.omit(cbind(x,y)), "na.action")
            dropNA <- function(x, nas) {
                if(length(nas)) {
                    if (is.matrix(x)) x[-nas, , drop = FALSE] else x[-nas]
                } else x
            }
            .Call(C_cor, Rank(dropNA(x, nas)), Rank(dropNA(y, nas)),
                  na.method, method == "kendall")
        }
    } else if (na.method != 3L) {
        ## i.e., 1 or 4, i.e. "all.obs" or "everything":
	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
        .Call(C_cor, x, y, na.method, method == "kendall")
    }
    else { # rank correlations and "pairwise.complete.obs"; the hard case
         ## Based on contribution from Shigenobu Aoki.
         ## matrix
         if (is.null(y)) {
             ncy <- ncx <- ncol(x)
             if(ncx == 0) stop("'x' is empty")
             r <- matrix(0, nrow = ncx, ncol = ncy)
             ## 2.6.0 assumed the diagonal was 1, but not so for all NAs,
             ## nor single non-NA pairs.
             for (i in seq_len(ncx)) {
                 for (j in seq_len(i)) {
                     x2 <- x[,i]
                     y2 <- x[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     ## we've removed all NAs
                     r[i, j] <- if(any(ok)) .Call(C_cor, x2, y2, 1L, method == "kendall") else NA
                 }
             }
             r <- r + t(r) - diag(diag(r))
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(x)
             r
         }
         ## vector/matrix x vector/matrix
         else {
             if(length(x) == 0L || length(y) == 0L)
                 stop("both 'x' and 'y' must be non-empty")
             matrix_result <- is.matrix(x) || is.matrix(y)
	     if (!is.matrix(x)) x <- matrix(x, ncol=1L)
	     if (!is.matrix(y)) y <- matrix(y, ncol=1L)
             ncx <- ncol(x)
             ncy <- ncol(y)
             r <- matrix(0, nrow = ncx, ncol = ncy)
             for (i in seq_len(ncx)) {
                 for (j in seq_len(ncy)) {
                     x2 <- x[,i]
                     y2 <- y[,j]
                     ok <- complete.cases(x2, y2)
                     x2 <- rank(x2[ok])
                     y2 <- rank(y2[ok])
                     r[i, j] <- if(any(ok)) .Call(C_cor, x2, y2, 1L, method == "kendall") else NA
                 }
             }
	     rownames(r) <- colnames(x)
	     colnames(r) <- colnames(y)
             if(matrix_result) r else drop(r)
         }
     }
}

cov <- function(x, y = NULL, use = "everything",
                method = c("pearson", "kendall", "spearman"))
{
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
		      "everything", "na.or.complete"))
    if(is.na(na.method)) stop("invalid 'use' argument")
    method <- match.arg(method)
    if(is.data.frame(y)) y <- as.matrix(y)
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.matrix(x) && is.null(y))
        stop("supply both 'x' and 'y' or a matrix-like 'x'")
    ## non-atomic x should not be 'numeric', but in case a method messes up
    stopifnot(is.numeric(x) || is.logical(x), is.atomic(x))
    if(!is.null(y)) stopifnot(is.numeric(y) || is.logical(y), is.atomic(y))

    ## Rank transform
    Rank <- function(u) {
        ## take care not to drop dims on a 0- or 1-row matrix
        if(length(u) == 0L) u
        else if(is.matrix(u)) {
            if(nrow(u) > 1L) apply(u, 2L, rank, na.last="keep") else row(u)
        } else rank(u, na.last="keep")
    }

    if(method == "pearson")
	.Call(C_cov, x, y, na.method, method == "kendall")
    else if (na.method %in% c(2L, 5L)) { ## "complete.obs"  or  "na.or.complete"

        if (is.null(y)) {
            .Call(C_cov, Rank(na.omit(x)), NULL, na.method,
                  method == "kendall")
        } else {
            nas <- attr(na.omit(cbind(x,y)), "na.action")
            dropNA <- function(x, nas) {
                if(length(nas)) {
                    if (is.matrix(x)) x[-nas, , drop = FALSE] else x[-nas]
                } else x
            }
            .Call(C_cov, Rank(dropNA(x, nas)), Rank(dropNA(y, nas)),
                  na.method, method == "kendall")
        }
    } else if (na.method != 3L) { ## 1 or 4: "all.obs"  or  "everything"
	x <- Rank(x)
	if(!is.null(y)) y <- Rank(y)
	.Call(C_cov, x, y, na.method, method == "kendall")
    }
    else ##  "pairwise.complete.obs"
	stop("cannot handle 'pairwise.complete.obs'")
}

var <- function(x, y = NULL, na.rm = FALSE, use) {
    if(missing(use))
	use <- if(na.rm) "na.or.complete" else "everything"
    na.method <-
	pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
		      "everything", "na.or.complete"))
    if(is.na(na.method)) stop("invalid 'use' argument")
    if (is.data.frame(x)) x <- as.matrix(x) else stopifnot(is.atomic(x))
    if (is.data.frame(y)) y <- as.matrix(y) else stopifnot(is.atomic(y))
    .Call(C_cov, x, y, na.method, FALSE)
}

cov2cor <- function(V)
{
    ## Purpose: Covariance matrix |--> Correlation matrix -- efficiently
    ## ----------------------------------------------------------------------
    ## Arguments: V: a covariance matrix (i.e. symmetric and positive definite)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 12 Jun 2003, 11L:50
    p <- (d <- dim(V))[1L]
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
