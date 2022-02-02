#  File src/library/base/R/sapply.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

##' "Simplify" a list of commonly structured components into an array.
##'
##' @title simplify list() to an array if the list elements are structurally equal
##' @param x a list, typically resulting from lapply()
##' @param higher logical indicating if an array() of "higher rank"
##'  should be returned when appropriate, namely when all elements of
##' \code{x} have the same \code{\link{dim}()}ension.
##' @return x itself, or an array if the simplification "is sensible"
simplify2array <- function(x, higher = TRUE, except = c(0L, 1L))
{
    if(!length(x)) return(x)
    if(length(common.len <- unique(lengths(x))) > 1L)
        return(x)
    except <- as.integer(except)
    ## For now, only allow historical exceptions for common length 0 or 1:
    except <- except[except <= 1L]
    if((common.len == 1L) && (1L %in% except)) {
        n <- length(x)
        r <- unlist(x, recursive = FALSE)
        if(length(r) == n) r else x
    }
    else if(!(common.len %in% except)) {
        n <- length(x)
        ## make sure that array(*) will not call rep() {e.g. for 'call's}:
	r <- unlist(x, recursive = FALSE, use.names = FALSE)
        if(is.null(r))
            x
        else if(higher && length(c.dim <- unique(lapply(x, dim))) == 1 &&
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
    if(!isFALSE(simplify))
	simplify2array(answer, higher = (simplify == "array"))
    else answer
}

vapply <- function(X, FUN, FUN.VALUE, ...,  USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    if(!is.vector(X) || is.object(X)) X <- as.list(X)
    .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
}


