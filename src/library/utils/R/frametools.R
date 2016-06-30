#  File src/library/utils/R/frametools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

stack <- function(x, ...) UseMethod("stack")

stack.data.frame <- function(x, select, drop=FALSE, ...)
{
    if (!missing(select)) {
	nl <- as.list(1L:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select),nl, parent.frame())
        x <- x[, vars, drop=FALSE]
    }
    keep <- unlist(lapply(x, is.vector))
    if(!sum(keep)) stop("no vector columns were selected")
    if(!all(keep))
        warning("non-vector columns will be ignored")
    x <- x[, keep, drop = FALSE]
    ind <- rep(factor(names(x), unique(names(x))), lengths(x))
    if (drop) {
        ind <- droplevels(ind)
    }
    data.frame(values = unlist(unname(x)),
               ind,
               stringsAsFactors = FALSE)
}

stack.default <- function(x, drop=FALSE, ...)
{
    x <- as.list(x)
    keep <- unlist(lapply(x, is.vector))
    if(!sum(keep)) stop("at least one vector element is required")
    if(!all(keep)) warning("non-vector elements will be ignored")
    x <- x[keep]
    ind <- rep(factor(names(x), unique(names(x))), lengths(x))
    if (drop) {
        ind <- droplevels(ind)
    }
    data.frame(values = unlist(unname(x)),
               ind,
               stringsAsFactors = FALSE)
}

unstack <- function(x, ...) UseMethod("unstack")

unstack.data.frame <- function(x, form, ...)
{
    form <- if(missing(form)) stats::formula(x) else stats::as.formula(form)
    if (length(form) < 3)
        stop("'form' must be a two-sided formula")
    res <- c(tapply(eval(form[[2L]], x), eval(form[[3L]], x), as.vector))
    if (length(res) >= 2L && any(diff(lengths(res)) != 0L))
        return(res)
    data.frame(res, stringsAsFactors = FALSE)
}

unstack.default <- function(x, form, ...)
{
    x <- as.list(x)
    form <- stats::as.formula(form)
    if ((length(form) < 3) || (length(all.vars(form))>2))
        stop("'form' must be a two-sided formula with one term on each side")
    res <- c(tapply(eval(form[[2L]], x), eval(form[[3L]], x), as.vector))
    if (length(res) >= 2L && any(diff(lengths(res)) != 0L))
        return(res)
    data.frame(res, stringsAsFactors = FALSE)
}
