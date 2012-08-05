#  File src/library/methods/R/BasicFunsList.R
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

## Lists of functions and expressions used in dispatch of functions
## defined internally (as .Primitive's) for which formal argument lists
## are not available, or for which a generic, if created,
## needs to have a special form (e.g., belonging to one of the
## predefined groups of functions).

## The list is expanded in .makeBasicFuns by adding the S4 group generics
## and the remaining primitives.

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
, "c" = function(x, ..., recursive = FALSE) standardGeneric("c")
, "all" = function(x, ..., na.rm = FALSE) standardGeneric("all")
, "any" = function(x, ..., na.rm = FALSE) standardGeneric("any")
, "sum" = function(x, ..., na.rm = FALSE) standardGeneric("sum")
, "prod" = function(x, ..., na.rm = FALSE) standardGeneric("prod")
, "max" = function(x, ..., na.rm = FALSE) standardGeneric("max")
, "min" = function(x, ..., na.rm = FALSE) standardGeneric("min")

, "range" = function(x, ..., na.rm = FALSE) standardGeneric("range")
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
    function(funslist, f, fdef, group = list())
{
    signature <- attr(fdef, "signature") #typically NULL, but see the case for "$"
    deflt <- get(f, "package:base")
    ## use the arguments of the base package function
    ##FIXME:  should also deal with the functions having ... as the first
    ## argument, but needs to create a generic with different args from the deflt
    ## => constructing a call to the base function from the default
    if(is.primitive(deflt)) {
        body(fdef, envir = globalenv()) <-
            substitute(standardGeneric(FNAME, DEFLT), list(FNAME=f, DEFLT=deflt))
    }
    else {
        fdef <- deflt
        body(fdef, envir = globalenv()) <-
            substitute(standardGeneric(FNAME), list(FNAME=f))
    }
    deflt <- .derivedDefaultMethod(deflt)
    elNamed(funslist, f) <- makeGeneric(f, fdef, deflt, group = group, package = "base",
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

genericForPrimitive <- function(f, where = topenv(parent.frame()), mustFind = TRUE) {
#    if(.matchBasic(f, .ExcludePrimitiveGenerics, FALSE))
#        stop(gettextf("methods may not be defined for primitive function %s in this version of R", sQuote(f)), domain = NA)
    env <- .findBasicFuns(where)
    funs <- get(".BasicFunsList", envir = env)
    ans <- elNamed(funs, f)
    ## this element may not exist (yet, during loading), dom't test null
    if(mustFind && identical(ans, FALSE))
        stop(gettextf("methods may not be defined for primitive function %s in this version of R",
                      sQuote(f)),
             domain = NA)
    ans
}

.findBasicFuns <- function(where) {
    allWhere <- .findAll(".BasicFunsList", where = where)
    if(length(allWhere) == 0)
        .methodsNamespace
    else
        as.environment(allWhere[[1L]])
}
