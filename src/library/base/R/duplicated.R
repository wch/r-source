#  File src/library/base/R/duplicated.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

duplicated <- function(x, incomparables = FALSE, ...) UseMethod("duplicated")

duplicated.default <-
    function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...)
    .Internal(duplicated(x, incomparables, fromLast,
                         if(is.factor(x)) min(length(x), nlevels(x) + 1L) else nmax))

duplicated.data.frame <-
    function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
    {
      
      n <- ncol(x)
      nmx <- names(x)
      nmincomparables <- names(incomparables)
      lincomparables <- length(incomparables)
      if(is.null(nmincomparables)) {
        if(lincomparables < n) {
          # pad any incomparables lists with the default value if list is shorter than the number columns in the supplied data.frame
          tmp <- c(incomparables, as.list(rep_len(FALSE, n - lincomparables)))
          names(tmp) <- nmx
          incomparables <- tmp 
        }
        if(lincomparables > n) {
          # if the list is unnamed and there are more elements in the list than there are columns, then only first n elements
          warning(paste("more columns in 'incomparables' than x, only using the first", n, "elements"))
          incomparables <- incomparables[1:n]
        }
      } else {
        # for named lists, find match, else default value
        tmp <- as.list(rep_len(FALSE, n))
        names(tmp) <- nmx
        i <- match(nmincomparables, nmx, 0L)
        if(any(i <= 0L))
          warning("not all columns named in 'incomparables' exist")
        tmp[ i[i > 0L] ] <- incomparables[i > 0L]
        incomparables <- tmp[nmx]
      }
      
      # first determine duplicates, then override when an incomparable value is found in a row since the existence of even 1 incomparable value in a row means it cannot be a duplicate
      res <- duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
      
      #for better performance only bother with the columns that have incomparable values not set to the default: !identical(x, FALSE)
      run_incomp_check <- sapply(incomparables, FUN=function(x){!identical(x, FALSE)})
      if (sum(run_incomp_check) > 0L){
        incomp_check <- mapply(FUN=function(column,incomparables){match(column, incomparables)}, x[run_incomp_check], incomparables[run_incomp_check])
        # any rows with an incomparable match means, TRUE, it can override the duplicated result
        overwrite <- apply(data.frame(incomp_check), 1, function(x){any(!is.na(x))})
        res[overwrite] <- FALSE
      }
      
      return(res)
      
    } else if(length(x) != 1L)
      {
        duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
    } else 
      {
        duplicated(x[[1L]], fromLast = fromLast, ...)
      }
}

duplicated.matrix <- duplicated.array <-
    function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop(gettextf("MARGIN = %d is invalid for dim = %d", MARGIN, dx),
             domain = NA)
    collapse <- (ndim > 1L) && (prod(dx[-MARGIN]) > 1L)
    temp <- if(collapse) apply(x, MARGIN, function(x) paste(x, collapse = "\r")) else x
    res <- duplicated.default(temp, fromLast = fromLast, ...)
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}

anyDuplicated <- function(x, incomparables = FALSE, ...)
    UseMethod("anyDuplicated")

anyDuplicated.default <-
    function(x, incomparables = FALSE, fromLast = FALSE, ...)
    .Internal(anyDuplicated(x, incomparables, fromLast))


anyDuplicated.data.frame <-
    function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    anyDuplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
}

anyDuplicated.matrix <- anyDuplicated.array <-
    function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop(gettextf("MARGIN = %d is invalid for dim = %d", MARGIN, dx),
             domain = NA)
    collapse <- (ndim > 1L) && (prod(dx[-MARGIN]) > 1L)
    temp <- if(collapse) apply(x, MARGIN, function(x) paste(x, collapse = "\r")) else x
    anyDuplicated.default(temp, fromLast = fromLast)
}

unique <- function(x, incomparables = FALSE, ...) UseMethod("unique")


## NB unique.default is used by factor to avoid unique.matrix,
## so it needs to handle some other cases.
unique.default <-
    function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...)
{
    if(is.factor(x)) {
        z <- .Internal(unique(x, incomparables, fromLast,
                              min(length(x), nlevels(x) + 1L)))
 	return(factor(z, levels = seq_len(nlevels(x)), labels = levels(x),
               ordered = is.ordered(x)))
    }
    z <- .Internal(unique(x, incomparables, fromLast, nmax))
    if(inherits(x, "POSIXct"))
        structure(z, class = class(x), tzone = attr(x, "tzone"))
    else if(inherits(x, "Date"))
        structure(z, class = class(x))
    else z
}

unique.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x, fromLast = fromLast, ...),  , drop = FALSE]
}

unique.matrix <- unique.array <-
    function(x, incomparables = FALSE , MARGIN = 1, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop(gettextf("MARGIN = %d is invalid for dim = %d", MARGIN, dx),
             domain = NA)
    collapse <- (ndim > 1L) && (prod(dx[-MARGIN]) > 1L)
    temp <- if(collapse) apply(x, MARGIN, function(x) paste(x, collapse = "\r")) else x
    args <- rep(alist(a=), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated.default(temp, fromLast = fromLast, ...)
    do.call("[", c(list(x), args, list(drop = FALSE)))
}
