#  File src/library/methods/R/BasicFunsList.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

## Lists of functions and expressions used in dispatch of functions
## defined internally (as .Primitive's) for which formal argument lists
## are not available, or for which a generic, if created,
## needs to have a special form (e.g., belonging to one of the
## predefined groups of functions).

##' The list is expanded in .makeBasicFuns() -> ./makeBasicFunsList.R by
##' adding the S4 group generics and the remaining primitives.
.BasicFunsList <-
list(
### subset/subassignment ops are regarded as language elements
"$" = structure(function(x, name)
{
    name <- as.character(substitute(name))
    standardGeneric("$")
}, signature = c("x"))
, "$<-" = structure(function(x, name, value)
{
    name <- as.character(substitute(name))
    standardGeneric("$<-")
}, signature = c("x", "value"))
, "[" = function(x, i, j, ..., drop = TRUE) standardGeneric("[")
, "[<-" = function(x, i, j, ..., value) standardGeneric("[<-")
, "[[" = function(x, i, j, ...) standardGeneric("[[")
, "[[<-" = function(x, i, j, ..., value) standardGeneric("[[<-")
### S4 generic via R_possible_dispatch in do_matprod
, "%*%" = function(x, y) standardGeneric("%*%")
, "xtfrm" = function(x) standardGeneric("xtfrm")
### these have a different arglist from the primitives
, "c" = structure(function(x, ...) standardGeneric("c"), signature="x")
, "all" = structure(function(x, ..., na.rm = FALSE) standardGeneric("all"),
                    signature="x")
, "any" = structure(function(x, ..., na.rm = FALSE) standardGeneric("any"),
                    signature="x")
, "sum" = structure(function(x, ..., na.rm = FALSE) standardGeneric("sum"),
                    signature="x")
, "prod" = structure(function(x, ..., na.rm = FALSE) standardGeneric("prod"),
                    signature="x")
, "max" = structure(function(x, ..., na.rm = FALSE) standardGeneric("max"),
                    signature="x")
, "min" = structure(function(x, ..., na.rm = FALSE) standardGeneric("min"),
                    signature="x")
, "range" = structure(function(x, ..., na.rm = FALSE) standardGeneric("range"),
                    signature="x")
## , "!" = function(e1) standardGeneric("!")
)

## the names of the basic funs with the style of "["
## R implements these in an inconsistent call mechanism, in which missing arguments
## are allowed, and significant, but argument names are not used.  See callNextMethod

.BasicSubsetFunctions <- c("[", "[[", "[<-", "[[<-")

## create generic functions corresponding to the basic (primitive) functions
## but don't leave them as generics in the package.  Instead store them in
## a named list to be used by setMethod, w/o forcing method dispatch on these
## functions.

.addBasicGeneric <-
    function(funslist, f, fdef, group = list(), internal = FALSE,
             internalArgs = names(formals(deflt)))
{
    deflt <- .BaseNamespaceEnv[[f]]
    ## use the arguments of the base package function
    ##FIXME:  should also deal with the functions having ... as the first
    ## argument, but needs to create a generic with different args from the deflt
    ## => constructing a call to the base function from the default
    if(is.primitive(deflt)) {
        signature <- attr(fdef, "signature") #typically NULL, but see the case for "$"
        body(fdef, envir = topenv()) <-
            substitute(standardGeneric(FNAME, DEFLT), list(FNAME=f, DEFLT=deflt))
    }
    else {
        if (internal) {
	    ## "forgets" the *defaults* of arguments, e.g. the "any" of as.vector():
	    ## formals(deflt) <- setNames(rep(alist(x=), length(internalArgs)),
	    ##                            internalArgs)
            call <- as.call(c(as.name(f), lapply(internalArgs, as.name)))
            body(deflt, envir = baseenv()) <-
                substitute(.Internal(CALL), list(CALL=call))
        }
        fdef <- deflt
        body(fdef, envir = topenv()) <-
            substitute(standardGeneric(FNAME), list(FNAME=f))
    }
    deflt <- .derivedDefaultMethod(deflt, internal = if (internal) f)
    if (internal) {
        signature <- names(formals(deflt))[1L]
    }
    funslist[[f]] <- makeGeneric(f, fdef, deflt, group = group, package = "base",
                                 signature = signature)
    funslist
}

.ShortPrimitiveSkeletons <-
    list( quote(f(x,i)), quote(fgets(x,i,value=value)))

.EmptyPrimitiveSkeletons <-
    list( quote(f(x)), quote(fgets(x,value=value)))

## utilities to get and set the primitive generics.
## Version below uses the environment, not the list
## in order to work with namespace for methods package
# genericForPrimitive <- function(f, where = topenv(parent.frame())) {
#     what <- methodsPackageMetaName("G", f)
#     if(exists(what, where))
#         get(what, where)
#     else
#         NULL
# }

# setGenericForPrimitive <-function(f, value, where = topenv(parent.frame()))
#     assign(methodsPackageMetaName("G", f), value, where)

## temporary versions while primitives are still handled by a global table

isBaseFun <- function(fun) {
    is.primitive(fun) || identical(environment(fun), .BaseNamespaceEnv)
}

inBasicFuns <- function(f) {
    !is.null(.BasicFunsList[[f]])
}

dispatchIsInternal <- function(fdef) {
    is.primitive(fdef@default) || is(fdef@default, "internalDispatchMethod")
}

genericForBasic <- function(f, where = topenv(parent.frame()),
                            mustFind = TRUE)
{
    ans <- .BasicFunsList[[f]]
    ## this element may not exist (yet, during loading), don't test null
    if(mustFind && isFALSE(ans))
        stop(gettextf("methods may not be defined for primitive function %s in this version of R",
                      sQuote(f)),
             domain = NA)
    ans
}
