#  File src/library/base/R/as.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

as.single <- function(x,...) UseMethod("as.single")
as.single.default <- function(x,...)
    structure(.Internal(as.vector(x,"double")), Csingle=TRUE)

# as.character is now internal.  The default method remains here to
# preserve the semantics that for a call with an object argument
# dispatching is done first on as.character and then on as.vector.
as.character.default <- function(x,...) .Internal(as.vector(x, "character"))

as.expression <- function(x,...) UseMethod("as.expression")
as.expression.default <- function(x,...) .Internal(as.vector(x, "expression"))

as.list <- function(x,...) UseMethod("as.list")
## This if() avoid dispatch on methods for as.vector.
as.list.default <- function (x, ...)
    if (typeof(x) == "list") x else .Internal(as.vector(x, "list"))

as.list.function <- function (x, ...) c(formals(x), list(body(x)))

## FIXME: as.vector(., "list")  should work for data.frames!
as.list.data.frame <- function(x,...) {
    x <- unclass(x)
    attr(x,"row.names") <- NULL
    x
}

as.list.environment <- function(x, all.names=FALSE, sorted=FALSE, ...)
    .Internal(env2list(x, all.names, sorted))

## NB: as.vector is used for several other as.xxxx, including
## as.expression, as.list, as.pairlist, as.single, as.symbol.
## as.vector dispatches internally so no need for a generic
as.vector <- function(x, mode = "any") .Internal(as.vector(x, mode))

as.matrix <- function(x, ...) UseMethod("as.matrix")
as.matrix.default <- function(x, ...) {
    if (is.matrix(x)) x
    else
	array(x, c(length(x), 1L),
	      if(!is.null(names(x))) list(names(x), NULL) else NULL)
}
as.null <- function(x,...) UseMethod("as.null")
as.null.default <- function(x,...) NULL

as.function <- function(x,...) UseMethod("as.function")
as.function.default <- function (x, envir = parent.frame(), ...)
    if (is.function(x)) x else .Internal(as.function.default(x, envir))

as.array <- function(x, ...) UseMethod("as.array")
as.array.default <- function(x, ...)
{
    if(is.array(x)) return(x)
    n <- names(x)
    dim(x) <- length(x)
    if(length(n)) dimnames(x) <- list(n)
    return(x)
}

as.symbol <- function(x) .Internal(as.vector(x, "symbol"))
as.name <- as.symbol
## would work too: as.name <- function(x) .Internal(as.vector(x, "name"))

as.qr <- function(x) stop("you cannot be serious", domain = NA)
