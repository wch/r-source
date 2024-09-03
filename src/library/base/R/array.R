#  File src/library/base/R/array.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

array <-
function(data = NA, dim = length(data), dimnames = NULL)
{
    ## allow for as.vector.factor (converts to character)
    if(is.atomic(data) && !is.object(data))
        return(.Internal(array(data, dim, dimnames)))
    data <- as.vector(data)
    ## package rv has an as.vector() method which leave this as a classed list
    if(is.object(data)) {
        dim <- as.integer(dim)
        if (!length(dim)) stop("'dim' cannot be of length 0")
        vl <- prod(dim)
        if(length(data) != vl) {
            ## C code allows long vectors, but rep() does not.
            if(vl > .Machine$integer.max)
                stop("'dim' specifies too large an array")
            data <- rep_len(data, vl)
        }
        if(length(dim)) dim(data) <- dim
        if(is.list(dimnames) && length(dimnames)) dimnames(data) <- dimnames
        data
    } else .Internal(array(data, dim, dimnames))
}

slice.index <-
function(x, MARGIN)
{
    d <- dim(x)
    if(is.null(d))
        d <- length(x)
    n <- length(d)

    ## Extract the margins and associated dimnames

    if (is.character(MARGIN)) {
        dn <- dimnames(x)
        if(is.null(dnn <- names(dn))) # names(NULL) is NULL
           stop("'x' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }


    if(!length(MARGIN) || any(MARGIN < 1L) || any(MARGIN > n))
        stop("incorrect value for 'MARGIN'")

    if(any(d == 0L)) return(array(integer(), d))

    m <- MARGIN[1L]
    y <- rep.int(rep.int(1L:d[m],
                         prod(d[seq_len(m - 1L)]) *
                         rep.int(1L, d[m])),
                 prod(d[seq.int(from = m + 1L,
                                length.out = n - m)]))
    if(length(MARGIN) > 1L) {
        p <- d[m]
        for(m in MARGIN[-1L]) {
            y <- y + p * rep.int(rep.int(seq.int(0L, d[m] - 1L),
                                         prod(d[seq_len(m - 1L)]) *
                                         rep.int(1L, d[m])),
                                 prod(d[seq.int(from = m + 1L,
                                                length.out = n - m)]))
            p <- p * d[m]
        }
    }

    dim(y) <- d
    y
}

provideDimnames <-
function(x, sep = "", base = list(LETTERS), unique = TRUE)
{
    ## provide dimnames where missing - not copying x unnecessarily
    dx <- dim(x)
    dnx <- dimnames(x)
    if(new <- is.null(dnx))
	dnx <- vector("list", length(dx))
    k <- length(M <- lengths(base))
    for(i in which(vapply(dnx, is.null, NA))) {
	ii <- 1L+(i-1L) %% k # recycling
        ss <- seq_len(dx[i]) - 1L # dim could be zero
	bi <- base[[ii]][1L+ (ss %% M[ii])]
	dnx[[i]] <- if(unique) make.unique(bi, sep = sep) else bi
	new <- TRUE
    }
    if(new) dimnames(x) <- dnx
    x
}

## The array split part used by apply() { ./apply.R }
## (With 'X' replaced by 'x').

asplit <-
function(x, MARGIN, drop = FALSE)
{
    ## Ensure that x is an array object
    dl <- length(dim(x))
    if(!dl) stop("dim(x) must have a positive length")
    if(is.object(x))
        x <- if(dl == 2L) as.matrix(x) else as.array(x)
    ## now record dim as coercion can change it
    ## (e.g. when a data frame contains a matrix).
    d <- dim(x)
    dn <- dimnames(x)
    ds <- seq_len(dl)

    ## Extract the margins and associated dimnames

    if (is.character(MARGIN)) {
        if(is.null(dnn <- names(dn))) # names(NULL) is NULL
           stop("'x' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    d.call <- d[-MARGIN]
    d.ans  <- d[ MARGIN]
    if (anyNA(d.call) || anyNA(d.ans))
        stop("'MARGIN' does not match dim(X)")
    s.call <- ds[-MARGIN]
    s.ans  <- ds[ MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans  <- dn[ MARGIN]
    dimnames(x) <- NULL

    .Internal(asplit(aperm(x, c(s.call, s.ans)),
                     d.ans, d.call, dn.ans, dn.call,
                     prod(d.call), prod(d.ans),
                     as.logical(drop)))
}

## Convert to data frame, mainly for list arrays produced by tapply()

array2DF <-
function (x, responseName = "Value", sep = "",
          base = list(LETTERS),
          simplify = TRUE, allowLong = TRUE)
{
    .df_helper <- function(x) # for data frames
    {
        ## check whether all components of list array 'x' 
        ## - are data frames
        ## - have same column names
        ## If TRUE, return value is a vector of corresponding nrow()-s 
        ## If FALSE, return value is integer(0)
        if (!is.list(x)) return(integer(0))
        if (!all(vapply(x, inherits, TRUE, "data.frame"))) return(integer(0))
        if (length(unique(vapply(x, ncol, 1L))) > 1L) return(integer(0))
        if (length(unique(lapply(x, colnames))) > 1L) return(integer(0))
        return(vapply(x, nrow, 1L))
    }
    .unvec_helper <- function(x) # for unnamed vectors
    {
        ## check whether all components of list array 'x' 
        ## - are atomic vectors
        ## - have no names
        ## If TRUE, return value is a vector of corresponding nrow()-s 
        ## If FALSE, return value is integer(0)
        if (!is.list(x)) return(integer(0))
        if (!all(vapply(x, is.atomic, TRUE))) return(integer(0))
        if (!all(vapply(x, function(v) is.null(names(v)), TRUE))) return(integer(0))
        return(vapply(x, length, 1L))
    }
    keys <-
        do.call(expand.grid,
                c(dimnames(provideDimnames(x, sep = sep, base = base)),
                  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
    vals <- NULL
    if(simplify) {
        ## handle data frames with identical colnames
        dfrows <- .df_helper(x)
        if (length(dfrows))
            return(cbind(keys[ rep(seq_along(dfrows), dfrows), , drop = FALSE],
                         do.call(rbind, x)))
        ## handle unnamed vectors
        if (allowLong) {
            unvecrows <- .unvec_helper(x)
            if (length(unvecrows))
                return(cbind(keys[ rep(seq_along(unvecrows), unvecrows), , drop = FALSE],
                             structure(data.frame(V = do.call(c, x)),
                                       names = responseName)))
        }
        ## handle generic case
        x <- simplify2array(c(x))
        if(is.array(x)) {
            vals <- asplit(x, 1L)
            if(is.null(names(vals)))
                names(vals) <-
                    paste0(responseName, sep, seq_along(vals))
        }
    }
    if(is.null(vals)) {
        vals <- list(c(x))
        names(vals) <- responseName
    }
    cbind(keys, list2DF(vals))
}
