#  File src/library/stats/R/cmdscale.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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
#  https://www.R-project.org/Licenses/

cmdscale <- function (d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE,
		      list. = eig || add || x.ret)
{
    if (anyNA(d))
	stop("NA values not allowed in 'd'")
    if(!list.) {
	if (eig)  warning(  "eig=TRUE is disregarded when list.=FALSE")
	if(x.ret) warning("x.ret=TRUE is disregarded when list.=FALSE")
    }
    if (is.null(n <- attr(d, "Size"))) {
        if(add) d <- as.matrix(d)
	x <- as.matrix(d^2)
        storage.mode(x) <- "double"
	if ((n <- nrow(x)) != ncol(x))
	    stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    } else { # d is  dist()-like  object
        rn <- attr(d, "Labels")
	x <- matrix(0, n, n) # must be double
        if (add) d0 <- x
	x[row(x) > col(x)] <- d^2
	x <- x + t(x)
        if (add) {
            d0[row(x) > col(x)] <- d
            d <- d0 + t(d0)
        }
    }
    n <- as.integer(n)
    ## we need to handle nxn internally in dblcen
    if(is.na(n) || n > 46340)
	stop(gettextf("invalid value of %s", "'n'"), domain = NA)
    if((k <- as.integer(k)) > n - 1 || k < 1)
        stop("'k' must be in {1, 2, ..  n - 1}")
    ## NB: this alters argument x, which is OK as it is re-assigned.
    x <- .Call(C_DoubleCentre, x)

    if(add) { ## solve the additive constant problem
        ## it is c* = largest eigenvalue of 2 x 2 (n x n) block matrix Z:
        i2 <- n + (i <- 1L:n)
        Z <- matrix(0, 2L*n, 2L*n)
        Z[cbind(i2,i)] <- -1
        Z[ i, i2] <- -x
        Z[i2, i2] <- .Call(C_DoubleCentre, 2*d)
        e <- eigen(Z, symmetric = FALSE, only.values = TRUE)$values
        add.c <- max(Re(e))
        ## and construct a new x[,] matrix:
	x <- matrix(double(n*n), n, n)
        non.diag <- row(d) != col(d)
        x[non.diag] <- (d[non.diag] + add.c)^2
        x <- .Call(C_DoubleCentre, x)
    }
    e <- eigen(-x/2, symmetric = TRUE)
    ev <- e$values[seq_len(k)]
    evec <- e$vectors[, seq_len(k), drop = FALSE]
    k1 <- sum(ev > 0)
    if(k1 < k) {
        warning(gettextf("only %d of the first %d eigenvalues are > 0", k1, k),
                domain = NA)
        evec <- evec[, ev > 0,  drop = FALSE]
        ev <- ev[ev > 0]
    }
    points <- evec * rep(sqrt(ev), each=n)
    dimnames(points) <- list(rn, NULL)
    if (list.) {
        evalus <- e$values # Cox & Cox have sum up to n-1, though
        list(points = points, eig = if(eig) evalus, x = if(x.ret) x,
             ac = if(add) add.c else 0,
             GOF = sum(ev)/c(sum(abs(evalus)), sum(pmax(evalus, 0))) )
    } else points
}
