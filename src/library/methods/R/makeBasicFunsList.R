#  File src/library/methods/R/makeBasicFunsList.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## the executable code to complete the generics corresponding to primitives,
## and to define the group generics for these functions.

## uses the primitive list and the function .addBasicGeneric
## defined (earlier) in BasicFunsList.R

utils::globalVariables(".addBasicGeneric")

.makeBasicFuns <- function(where)
{
    funs <- get(".BasicFunsList", envir=where)

    ## First, set up the existing functions in the list as valid generics.
    ## This will override anything except the S4 group generics.
    curNames <- names(funs)
    for(i in seq_along(funs)) {
	val <- funs[[i]]
        if (is.function(val))
            funs <- .addBasicGeneric(funs, curNames[[i]], val, "")
    }

    ## Next, add the remaining primitive generics
    prims <- names(.GenericArgsEnv)
    new_prims <- setdiff(prims, names(funs))
    for(nm in new_prims) {
        f <- .GenericArgsEnv[[nm]]
        body(f) <- substitute(standardGeneric(ff), list(ff=val))
        funs <- .addBasicGeneric(funs, nm, f, "")
    }

    ## Then add all the primitives that are not already there.
    ff <- as.list(baseenv(), all.names=TRUE)
    prims <- ff[vapply(ff, is.primitive, logical(1L))]
    new_prims <- setdiff(names(prims), names(funs))
    add <- rep(list(FALSE), length(new_prims))
    names(add) <- new_prims
    funs <- c(funs, add)

    ## the Math group.
    members <- c("abs", "sign", "sqrt",
		 "ceiling", "floor", "trunc",
		 "cummax", "cummin", "cumprod", "cumsum",
		 "exp", "expm1",
		 "log", "log10", "log2", "log1p",
		 "cos", "cosh", "sin", "sinh", "tan", "tanh",
		 "acos", "acosh", "asin", "asinh", "atan", "atanh",
		 "cospi", "sinpi", "tanpi",
		 "gamma", "lgamma", "digamma", "trigamma"
		 )
    for(f in members) {
	funs <-
	    .addBasicGeneric(funs, f,
			     if(f %in% c("log", "trunc")) {
				 function(x, ...) standardGeneric("")
			     } else   function(x) standardGeneric(""),
			     "Math")
    }

    setGroupGeneric(where=where, "Math", function(x)NULL,
		    knownMembers = members, package = "base")

    ## The Math2 group.
    funs <- .addBasicGeneric(funs, "round",
			     function(x, digits = 0) standardGeneric(""),
			     "Math2")
    funs <- .addBasicGeneric(funs, "signif",
			     function(x, digits = 6) standardGeneric(""),
			     "Math2")

    setGroupGeneric(where = where, "Math2", function(x, digits) NULL,
		    knownMembers = c("round", "signif"), package = "methods")

    ## The Arith group
    members <- c("+", "-", "*", "^", "%%", "%/%", "/")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Arith")

    setGroupGeneric(where = where, "Arith", function(e1, e2)NULL,
		    group = "Ops", knownMembers = members, package = "base")

    ## the Compare group
    members <- c("==", ">", "<", "!=", "<=", ">=")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Compare")

    setGroupGeneric(where = where, "Compare", function(e1, e2)NULL,
		    group = "Ops", knownMembers = members, package = "methods")

    ## The Logic group
    members <- c("&", "|") ## *not*  "!" since that has only one argument
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(e1, e2) standardGeneric(""),
				 "Logic")
    setGroupGeneric(where = where, "Logic", function(e1, e2) NULL,
		    group = "Ops", knownMembers = members, package = "base")

    ## the Ops group generic

    setGroupGeneric(where = where,"Ops", function(e1, e2) NULL,
		    knownMembers = c("Arith", "Compare", "Logic"),
                    package = "base")


    ## The Summary group

    ## These are a bit problematic, since they essentially have "..."
    ## as their only data-related formal argument.  The treatment
    ## since S3 has been to define the generic with a special first
    ## argument, to allow method dispatch.  But the method had better
    ## invoke the generic recursively or perform some other special
    ## computations, in order to avoid unintended anomalies, such as
    ## !identical(max(x,y), max(y,x))

    members <- c("max", "min", "range", "prod", "sum", "any", "all")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function (x, ..., na.rm = FALSE)
				 standardGeneric(""),
				 "Summary")

    setGroupGeneric(where = where, "Summary",
		    function(x, ..., na.rm = FALSE) NULL,
		    knownMembers = members, package = "base")

    ## The Complex group

    ## R adds this group to the previous S language function groups,
    ## for all the operations defined on complex numbers.  For
    ## applications wanting to define a new class that extends the
    ## concept of complex numbers, a function group is likely to be
    ## useful since all these functions may operate in a similar
    ## manner (by analogy, e.g., with the Math group).

    members <- c("Arg", "Conj", "Im", "Mod", "Re")
    for(f in members)
	funs <- .addBasicGeneric(funs, f, function(z) standardGeneric(""),
				 "Complex")

    setGroupGeneric(where=where,"Complex", function(z)NULL,
		    knownMembers = members, package = "base")

    funs <- .addBasicGeneric(funs, "unlist", internal=TRUE)
### TODO: bring back if/when .Internal(is.unsorted()) is useful to override
    ## funs <- .addBasicGeneric(funs, "is.unsorted", internal=TRUE,
    ##                          internalArgs=c("x", "strictly"))
    funs <- .addBasicGeneric(funs, "as.vector", internal=TRUE)

    assign(".BasicFunsList", funs, envir=where)
    rm(.addBasicGeneric, envir=where)
}


.initImplicitGenerics <- function(where)
{
    ## create implicit generics & possibly  methods for the functions in .BasicFunsList.

    setGeneric("with", signature = "data", where = where)
    setGenericImplicit("with", where, FALSE)

    ## when setMethod()ing on chol2inv, one should *not* have to deal with
    ## arguments  'size' and 'LINPACK' :
    setGeneric("chol2inv", function(x, ...) standardGeneric("chol2inv"),
	       useAsDefault = function(x, ...) base::chol2inv(x, ...),
	       signature = "x", where = where)
    setGenericImplicit("chol2inv", where, FALSE)

    setGeneric ("determinant", function(x, logarithm=TRUE, ...) standardGeneric("determinant"),
		useAsDefault = function(x, logarithm=TRUE, ...)
		base::determinant(x, logarithm, ...),
		signature = c("x", "logarithm"), where = where)
    setGenericImplicit("determinant", where, FALSE)

    setGeneric("rcond", function(x, norm, ...) standardGeneric("rcond"),
	       useAsDefault = function(x, norm, ...) base::rcond(x, norm, ...),
	       signature = c("x", "norm"), where = where)
    setGenericImplicit("rcond", where, FALSE)

    setGeneric("norm", function(x, type, ...) standardGeneric("norm"),
	       useAsDefault = function(x, type, ...) base::norm(x, type, ...),
	       signature = c("x", "type"), where = where)
    ## this method *belong*s to the generic:
    setMethod("norm", signature(x = "ANY", type = "missing"),
              function (x, type, ...) norm(x, type = "O", ...))
    setGenericImplicit("norm", where, FALSE)

    setGeneric("backsolve", function(r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE, ...)
	       standardGeneric("backsolve"),
	       useAsDefault =
	       function(r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE, ...)
	       base::backsolve(r, x, k = k,
			       upper.tri = upper.tri, transpose = transpose, ...),
	       signature = c("r", "x"), where = where)
    setGenericImplicit("backsolve", where, FALSE)

    setGeneric("colMeans", function(x, na.rm = FALSE, dims = 1, ...)
			standardGeneric("colMeans"),
	       useAsDefault = function(x, na.rm = FALSE, dims = 1, ...)
			base::colMeans(x, na.rm=na.rm, dims=dims, ...),
	       signature = c("x", "na.rm", "dims"), where = where)
    setGeneric("colSums", function(x, na.rm = FALSE, dims = 1, ...)
			standardGeneric("colSums"),
	       useAsDefault = function(x, na.rm = FALSE, dims = 1, ...)
			base::colSums(x, na.rm=na.rm, dims=dims, ...),
	       signature = c("x", "na.rm", "dims"), where = where)
    setGeneric("rowMeans", function(x, na.rm = FALSE, dims = 1, ...)
			standardGeneric("rowMeans"),
	       useAsDefault = function(x, na.rm = FALSE, dims = 1, ...)
			base::rowMeans(x, na.rm=na.rm, dims=dims, ...),
	       signature = c("x", "na.rm", "dims"), where = where)
    setGeneric("rowSums", function(x, na.rm = FALSE, dims = 1, ...)
			standardGeneric("rowSums"),
	       useAsDefault = function(x, na.rm = FALSE, dims = 1, ...)
			base::rowSums(x, na.rm=na.rm, dims=dims, ...),
	       signature = c("x", "na.rm", "dims"), where = where)
    setGenericImplicit("colMeans", where, FALSE)
    setGenericImplicit("colSums",  where, FALSE)
    setGenericImplicit("rowMeans", where, FALSE)
    setGenericImplicit("rowSums",  where, FALSE)

    setGeneric("crossprod", function(x, y = NULL, ...) standardGeneric("crossprod"),
	       useAsDefault = function(x, y = NULL, ...) base::crossprod(x, y),
	       signature = c("x", "y"), where = where)
    setGeneric("tcrossprod", function(x, y = NULL, ...) standardGeneric("tcrossprod"),
	       useAsDefault = function(x, y = NULL, ...) base::tcrossprod(x, y),
	       signature = c("x", "y"), where = where)
    setGenericImplicit("crossprod",  where, FALSE)
    setGenericImplicit("tcrossprod",  where, FALSE)

    setGeneric("sample", function(x, size, replace = FALSE, prob = NULL, ...)
			standardGeneric("sample"),
	       useAsDefault = function(x, size, replace = FALSE, prob = NULL, ...)
			base::sample(x, size, replace=replace, prob=prob, ...),
	       signature = c("x", "size"), where = where)
    setGenericImplicit("sample", where, FALSE)

    ## qr.R(): signature should only have "qr", args should have "..."
    setGeneric("qr.R", function(qr, complete = FALSE, ...) standardGeneric("qr.R"),
	       useAsDefault= function(qr, complete = FALSE, ...)
                   base::qr.R(qr, complete=complete),
	       signature = "qr", where = where)
    setGenericImplicit("qr.R", where, FALSE)

    ## our toeplitz() only has 'x'; want the generic "here" rather than "out there"
    setGeneric("toeplitz", function(x, ...) standardGeneric("toeplitz"),
	       useAsDefault= function(x, ...) stats::toeplitz(x),
	       signature = "x", where = where)
    setGenericImplicit("toeplitz", where, FALSE)

    ## svd(): signature should only have "x" (no 'nu', 'nv' ..)
    setGeneric("svd", function(x, ...) standardGeneric("svd"),
	       useAsDefault= function(x, ...) base::svd(x, ...),
	       signature = "x", where = where)
    setGenericImplicit("svd", where, FALSE)


    ## not implicitGeneric() which is not yet available "here"
    registerImplicitGenerics(where = where)
}
