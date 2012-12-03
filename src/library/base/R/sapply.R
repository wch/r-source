#  File src/library/base/R/sapply.R
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

##' "Simplify" a list of commonly structured components into an array.
##'
##' @title simplify list() to an array if the list elements are structurally equal
##' @param x a list, typically resulting from lapply()
##' @param higher logical indicating if an array() of "higher rank"
##'  should be returned when appropriate, namely when all elements of
##' \code{x} have the same \code{\link{dim}()}ension.
##' @return x itself, or an array if the simplification "is sensible"
simplify2array <- function(x, higher = TRUE)
{
    if(length(common.len <- unique(unlist(lapply(x, length)))) > 1L)
        return(x)
    if(common.len == 1L)
        unlist(x, recursive = FALSE)
    else if(common.len > 1L) {
        n <- length(x)
        ## make sure that array(*) will not call rep() {e.g. for 'call's}:
        r <- as.vector(unlist(x, recursive = FALSE))
        if(higher && length(c.dim <- unique(lapply(x, dim))) == 1 &&
           is.numeric(c.dim <- c.dim[[1L]]) &&
           prod(d <- c(c.dim, n)) == length(r)) {

            iN1 <- is.null(n1 <- dimnames(x[[1L]]))
            n2 <- names(x)
            dnam <-
                if(!(iN1 && is.null(n2)))
                    c(if(iN1) rep.int(list(n1), length(c.dim)) else n1,
                      list(n2)) ## else NULL
            array(r, dim = d, dimnames = dnam)

        } else if(prod(d <- c(common.len, n)) == length(r))
            array(r, dim = d,
                  dimnames = if(!(is.null(n1 <- names(x[[1L]])) &
                  is.null(n2 <- names(x)))) list(n1,n2))
        else x
    }
    else x
}

sapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    answer <- lapply(X = X, FUN = FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
	names(answer) <- X
    if(!identical(simplify, FALSE) && length(answer))
	simplify2array(answer, higher = (simplify == "array"))
    else answer
}

vapply <- function(X, FUN, FUN.VALUE, ...,  USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    if(!is.vector(X) || is.object(X)) X <- as.list(X)
    .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
}


