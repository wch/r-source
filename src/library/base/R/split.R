#  File src/library/base/R/split.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

split <- function(x, f, drop = FALSE, ...) UseMethod("split")

split.default <- function(x, f, drop = FALSE, sep = ".", lex.order = FALSE, ...)
{
    if(!missing(...)) .NotYetUsed(deparse(...), error = FALSE)

    if (is.list(f))
	f <- interaction(f, drop = drop, sep = sep, lex.order = lex.order)
    else if (!is.factor(f)) f <- as.factor(f) # docs say as.factor
    else if (drop) f <- factor(f) # drop extraneous levels
    storage.mode(f) <- "integer"  # some factors have had double in the past
    if (is.null(attr(x, "class")))
	return(.Internal(split(x, f)))
    ## else
    ind <- .Internal(split(seq_along(x), f))
    lapply(ind, function(i) x[i])
}

## This is documented to work for matrices too
split.data.frame <- function(x, f, drop = FALSE, ...)
{
    if (inherits(f, "formula")) f <- .formula2varlist(f, x)
    lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...),
           function(ind) x[ind, , drop = FALSE])
}

`split<-` <- function(x, f, drop = FALSE, ..., value) UseMethod("split<-")

`split<-.default` <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq_along(x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i] <- value[[j]]
    }
    x
}

## This is documented to work for matrices too
`split<-.data.frame` <- function(x, f, drop = FALSE, ..., value)
{
    if (inherits(f, "formula")) f <- eval(attr(stats::terms(f), "variables"), x, environment(f))
    ix <- split(seq_len(nrow(x)), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i,] <- value[[j]]
    }
    x
}

## (Note: use rep(NA_integer_, len) for indexing here. Logical NA confuses 
## tibbles and only coincidentally does the right thing otherwise, 
## because len is longer than value[[1]]. Otherwise recycling would kick in.) 
unsplit <- function (value, f, drop = FALSE)
{
    len <- length(if (is.list(f)) f[[1L]] else f)
    if (is.data.frame(value[[1L]])) {
        x <- value[[1L]][rep(NA_integer_, len),, drop = FALSE]
        rownames(x) <- unsplit(lapply(value, rownames), f, drop = drop)
    } else
        x <- value[[1L]][rep(NA_integer_, len)]
    split(x, f, drop = drop) <- value
    x
}

## utility to convert formula to list suitable for split(f = )

## Typical forms are ~ a + b and ~ list(a, b)

.formula2varlist <- function(formula, data, warnLHS = TRUE, ignoreLHS = warnLHS)
{
    if (!inherits(formula, "formula")) stop("'formula' must be a formula")
    if (!is.list(data) && !is.environment(data)) data <- as.data.frame(data)
    if (length(formula) == 3) {
        if (warnLHS) {
            if (ignoreLHS)
                warning("Unexpected LHS in 'formula' has been ignored.")
            else
                warning("Unexpected LHS in 'formula' has been combined with RHS.")
        }
        if (ignoreLHS) formula <- formula[-2]
    }
    ## If formula = ~list(...)
    if (length(formula[[2]]) > 1L && formula[[2]][[1]] == quote(list)) {
        ans <- eval(formula[[2]], data, environment(formula))
    }
    else {
        fterms <- stats::terms(formula)
        ans <- eval(attr(fterms, "variables"), data, environment(formula))
        names(ans) <- attr(fterms, "term.labels")
    }
    ans
}
