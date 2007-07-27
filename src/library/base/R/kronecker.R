#  File src/library/base/R/kronecker.R
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

kronecker <- function (X, Y, FUN = "*", make.dimnames = FALSE, ...)
{
    X <- as.array(X)
    Y <- as.array(Y)
    if (make.dimnames) {
	dnx <- dimnames(X)
	dny <- dimnames(Y)
    }
    dX <- dim(X)
    dY <- dim(Y)
    ld <- length(dX) - length(dY)
    if (ld < 0)
	dX <- dim(X) <- c(dX, rep.int(1, -ld))
    else if (ld > 0)
	dY <- dim(Y) <- c(dY, rep.int(1, ld))
    opobj <- outer(X, Y, FUN, ...)
    dp <- as.vector(t(matrix(1:(2*length(dX)), ncol = 2)[, 2:1]))
    opobj <- aperm(opobj, dp)
    dim(opobj) <- dX * dY

    if (make.dimnames && !(is.null(dnx) && is.null(dny))) {
	if (is.null(dnx))
	    dnx <- vector("list", length(dX))
	else if (ld < 0)
	    dnx <- c(dnx, vector("list", -ld))
	tmp <- which(sapply(dnx, is.null))
	dnx[tmp] <- lapply(tmp, function(i) rep.int("", dX[i]))

	if (is.null(dny))
	    dny <- vector("list", length(dY))
	else if (ld > 0)
	    dny <- c(dny, vector("list", ld))
	tmp <- which(sapply(dny, is.null))
	dny[tmp] <- lapply(tmp, function(i) rep.int("", dY[i]))

	k <- length(dim(opobj))
	dno <- vector("list", k)
	for (i in 1:k) {
	    tmp <- outer(dnx[[i]], dny[[i]], FUN="paste", sep=":")
	    dno[[i]] <- as.vector(t(tmp))
	}
	dimnames(opobj) <- dno
    }
    opobj
}

## Binary operator, hence don't simply do "%x%" <- kronecker.
"%x%" <- function(X, Y) kronecker(X, Y)
