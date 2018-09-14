#  File src/library/methods/R/as.R
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

as <-
  ## Returns the version of this object coerced to be the given `Class'.
  ## If the corresponding `is' relation is true, it will be used.  In particular,
  ## if the relation has a coerce method, the method will be invoked on `object'.
  ##
  ## If the `is' relation is FALSE, and `coerceFlag' is `TRUE',
  ## the coerce function will be called (which will throw an error if there is
  ## no valid way to coerce the two objects).  Otherwise, `NULL' is returned.
  function(object, Class, strict = TRUE, ext = possibleExtends(thisClass, Class))
{
    thisClass <- .class1(object)
    if(.identC(thisClass, Class) || .identC(Class, "ANY"))
        return(object)
    where <- .classEnv(thisClass, mustFind = FALSE)
    coerceFun <- getGeneric("coerce", where = where)
    ## get the methods table, use inherited table
    coerceMethods <- .getMethodsTable(coerceFun,environment(coerceFun),inherited= TRUE)
    asMethod <- .quickCoerceSelect(thisClass, Class, coerceFun, coerceMethods, where)
    if(is.null(asMethod)) {
        sig <-  c(from=thisClass, to = Class)
        ## packageSlot(sig) <- where
        ## try first for an explicit (not inherited) method
        ## ?? Can this ever succeed if .quickCoerceSelect failed?
        asMethod <- selectMethod("coerce", sig, optional = TRUE,
                                 useInherited = FALSE, #optional, no inheritance
                                 fdef = coerceFun, mlist = getMethodsForDispatch(coerceFun))
        if(is.null(asMethod)) {
            canCache <- TRUE
            inherited <- FALSE
            if(is(object, Class)) {
                ClassDef <- getClassDef(Class, where)
                ## use the ext information, computed or supplied
                if(isFALSE(ext))
                    stop(sprintf("internal problem in as(): %s is(object, \"%s\") is TRUE, but the metadata asserts that the 'is' relation is FALSE",
                                 dQuote(thisClass), Class),
                         domain = NA)
                else if(isTRUE(ext))
                    asMethod <- .makeAsMethod(quote(from), TRUE, Class, ClassDef, where)
                else {
                  test <- ext@test
                  asMethod <- .makeAsMethod(ext@coerce, ext@simple, Class, ClassDef, where)
                  canCache <- (!is(test, "function")) || isTRUE(body(test))
                 }
            }
            if(is.null(asMethod) && extends(Class, thisClass)) {
                ClassDef <- getClassDef(Class, where)
                asMethod <- .asFromReplace(thisClass, Class, ClassDef, where)
            }
            ## if none of these applies, look for an inherited method
            ## but only on the from argument
            if(is.null(asMethod)) {
                asMethod <- selectMethod("coerce", sig, optional = TRUE,
                                         c(from = TRUE, to = FALSE),
                                         fdef = coerceFun, mlist = coerceMethods)
                inherited <- TRUE
            }
            else if(canCache)  # make into method definition
                asMethod <- .asCoerceMethod(asMethod, thisClass, ClassDef, FALSE, where)
	    if(is.null(asMethod))
		stop(gettextf("no method or default for coercing %s to %s",
			      dQuote(thisClass),
                              dQuote(Class)),
                     domain = NA)
	    else if(canCache) {
		## cache in the coerce function's environment
		cacheMethod("coerce", sig, asMethod, fdef = coerceFun,
			    inherited = inherited)
	    }
        }
    }
    if(strict)
        asMethod(object)
    else
        asMethod(object, strict = FALSE)
}

.quickCoerceSelect <- function(from, to, fdef, methods, where) {
    if(is.null(methods))
        return(NULL)
    else if(is.environment(methods)) {
        method <- .findMethodInTable(c(from, to), methods)
        if(is.environment(method))
            NULL # FIXME:  should resolve by checking package
        else
            method
    }
    else {
        allMethods <- methods@allMethods
        i <- match(from, names(allMethods))
        if(is.na(i))
          NULL
        else {
            methodsi <- allMethods[[i]]
            j <- match(to, names(methodsi))
            if(is.na(j))
              NULL
            else
              methodsi[[j]]
        }
    }
}

.asFromReplace <- function(fromClass, toClass, ClassDef, where) {
    ## toClass extends fromClass, so an asMethod will
    ## be the equivalent of new("toClass", fromObject)
    ## But must check that replacement is defined, in the case
    ## of nonstandard superclass relations
    replaceMethod <- ClassDef@contains[[fromClass]]
    if(is(replaceMethod, "SClassExtension") &&
       !identical(as(replaceMethod@replace, "function"), .ErrorReplace)) {
        f <- function(from, to) NULL
        body(f, envir = where) <-
            substitute({obj <- new(TOCLASS); as(obj, FROMCLASS) <- from; obj},
                       list(FROMCLASS = fromClass, TOCLASS = toClass))
        f
    }
    else
        NULL

}



"as<-" <-
  ## Set the portion of the object defined by the right-hand side.
  ##
  ## Typically, the object being modified extends the class of the right-hand side object,
  ## and contains the slots of that object. These slots (only) will then be replaced.
  function(object, Class, value) {
    thisClass <- .class1(object)
    if(!.identC(.class1(value), Class))
        value <- as(value, Class, strict = FALSE)
    where <- .classEnv(class(object))
    coerceFun <- getGeneric("coerce<-", where = where)
    coerceMethods <- getMethodsForDispatch(coerceFun)
    asMethod <- .quickCoerceSelect(thisClass, Class, coerceFun, coerceMethods, where)
    if(is.null(asMethod)) {
        sig <- c(from = thisClass, to = Class)
        canCache <- TRUE
        inherited <- FALSE
        asMethod <- selectMethod("coerce<-", sig, TRUE, FALSE, #optional, no inheritance
                                 fdef = coerceFun, mlist = coerceMethods)
        if(is.null(asMethod)) {
            if(is(object, Class)) {
                asMethod <- possibleExtends(thisClass, Class)
                if(isTRUE(asMethod)) {# trivial, probably identical classes
                    class(value) <- class(object)
                    return(value)
                }
                else {
                    test <- asMethod@test
                    asMethod <- asMethod@replace
                    canCache <- (!is(test, "function")) || isTRUE(body(test))
                    if(canCache) { ##the replace code is a bare function
                        ClassDef <- getClassDef(Class, where)
                        asMethod <- .asCoerceMethod(asMethod, thisClass, ClassDef, TRUE, where)
                    }
                }
            }
            else { # search for inherited method
              asMethod <- selectMethod("coerce<-", sig, TRUE, c(from = TRUE, to = FALSE), doCache = TRUE)
              inherited <- TRUE
            }
        }
        ## cache for next call
        if(canCache && !is.null(asMethod))
                 cacheMethod("coerce<-", sig, asMethod, fdef = coerceFun,
                             inherited = inherited)
     }
    if(is.null(asMethod))
        stop(gettextf("no method or default for as() replacement of %s with Class=\"%s\"",
                      dQuote(thisClass),
                      Class),
             domain = NA)
    asMethod(object, Class, value)
}



setAs <-
  function(from, to, def, replace = NULL, where = topenv(parent.frame()))
{
    ## where there is an "is" relation, modify it
    fromDef <- getClassDef(from, where)
    extds <- possibleExtends(from, to, fromDef)
    if(is(extds, "SClassExtension")) {
        test <- extds@test
        if(is.null(replace))
            replace <- extds@replace
        test <- NULL
        setIs(from, to, test = test, coerce = def, replace = replace, where = where)
    }
    else if(isTRUE(extds)) {
        if(.identC(from, to))
            stop(gettextf("trying to set an 'as' relation from %s to itself",
                          dQuote(.class1(from))),
                 domain = NA)
        ## usually to will be a class union, where setAs() is not
        ## allowed by the definition of a union
        toDef <- getClassDef(to, where=where)
        if(is.null(toDef))
            stop(gettextf("class %s is not defined in this environment",
                          dQuote(to)),
                 domain = NA)
        if(isClassUnion(toDef))
            stop(gettextf("class %s is a class union: 'coerce' relations to a class union are not meaningful",
                          dQuote(to)),
                 domain = NA)
        ## else go ahead (but are there any cases here where extds is TRUE?)
        setIs(from, to, coerce = def, replace = replace, where = where)
    }
    ## else extds is FALSE -- no is() action
        args <- formalArgs(def)
        if(!is.na(match("strict", args))) args <- args[-match("strict", args)]
        if(length(args) == 1)
            def <- substituteFunctionArgs(def, "from", functionName = "coerce")
        else  if(length(args) != 2 || !identical(args, c("from", "to")))
               stop(gettextf("'as' method should have one argument, or match the arguments of coerce(): got  (%s)",
                           paste(formalArgs(def), collapse = ", ")),
                  domain = NA)
    ## coerce@.Data is the "prototype" from which we construct the method
        method <- as.list(coerce@.Data) # the function def'n, just to get arguments correct
        method$to <- to
        method <- as.function(method)
        body(method, envir = environment(def)) <- body(def)
        setMethod("coerce", c(from, to), method, where = where)
        if(!is.null(replace)) {
            args <- formalArgs(replace)
            if(identical(args, c("from", "to", "value")))
                method <- replace
            else {
                ## if not from an extends object, process the arguments
                if(length(args) != 2)
                    stop(gettextf("a 'replace' method definition in 'setAs' must be a function of two arguments, got %d", length(args)), domain = NA)
                replace <- body(replace)
                if(!identical(args, c("from", "value"))) {
                    ll <- list(quote(from), quote(value))
                    names(ll) <- args
                    replace <- substituteDirect(replace, ll)
                    warning(gettextf("argument names in 'replace' changed to agree with 'coerce<-' generic:\n%s", paste(deparse(replace), sep="\n    ")),
                            domain = NA)
                }
                method <- eval(function(from, to, value)NULL)
                body(method, envir = .GlobalEnv) <- replace
            }
            setMethod("coerce<-", c(from, to), method, where = where)
        }
}

.setCoerceGeneric <- function(where) {
  ## create the initial version of the coerce function, with methods that convert
  ## arbitrary objects to the basic classes by calling the corresponding as.<Class>
  ## functions.
  setGeneric("coerce", function(from, to, strict = TRUE) {
      if(TRUE) {
          warning("direct use of coerce() is deprecated:  use as(from, class(to)) instead", domain = NA)
          return(as(from, class(to), strict = strict))
      }
      standardGeneric("coerce")
      },
             where = where)
  setGeneric("coerce<-", function(from, to, value) {
      if(TRUE) {
          warning("direct use of coerce() is deprecated:  use as(from, class(to)) <- value instead", domain = NA)
          return(`as<-`(from, class(to), value))
      }
      standardGeneric("coerce<-")
      }, where = where)
  basics <- c(
 "POSIXct",  "POSIXlt", "Date",  "array",  "call",  "character",  "complex",  "data.frame",
 "double",
 "environment",  "expression",  "factor",  "formula",  "function",  "integer",
 "list",  "logical",  "matrix",  "name",  "numeric",  "ordered",
  "single",  "table",   "vector")
  basics <- basics[!is.na(match(basics,.BasicClasses))]
  for(what in basics) {
      ## if the class is a basic class and there exists an as.<class> function,
      ## use it as the coerce method.
      method  <- .basicCoerceMethod
      switch(what,
	     array =, matrix = body(method, envir = environment(method)) <-
	     substitute({
		 value <- AS(from)
		 if(strict) {
		     dm <- dim(value)
		     dn <- dimnames(value)
		     attributes(value) <- NULL
		     dim(value) <- dm
		     dimnames(value) <- dn
		 }
		 value
	     }, list(AS = as.name(paste0("as.", what)))),
	     ##
	     ts = body(method, envir = environment(method)) <- quote({
		 value <- as.ts(from)
		 if(strict) {
		     attributes(value) <- NULL
		     class(value) <- class(new("ts"))
		     tsp(value) <- tsp(from)
		 }
		 value
	     }),
	     ## default: no attributes
	     body(method, envir = environment(method)) <- substitute({
		 value <- AS(from)
		 if(strict)
		     attributes(value) <- NULL
		 value
	     }, list(AS = as.name(paste0("as.", what))))
	     )
      setMethod("coerce", c("ANY", what), method, where = where)
  }
  ## and some hand-coded ones
  body(method) <- quote(as.null(from))
  setMethod("coerce", c("ANY", "NULL"), method, where = where)
  body(method) <- quote({
            if(length(from) != 1)
                warning("ambiguous object (length != 1) to coerce to \"name\"")
            as.name(from)
        })
  setMethod("coerce", c("ANY","name"), method, where = where)
  ## Proposed on R-devel, Dec. 7, 2015, by JMC, this is too radical,
  ## coercing to "double" in too many cases where "numeric" data remained "integer":
  ## JMC, on Dec. 11 added that a setDataPart() special-case hack would be needed additionally
  ## setMethod("coerce", c("integer", "numeric"),
  ##           ## getMethod("coerce", c("ANY", "numeric"), where = envir) -- not yet available
  ##           function (from, to, strict = TRUE) {
  ##               value <- as.numeric(from)
  ##               if(strict)
  ##                   attributes(value) <- NULL
  ##               value
  ##           }, where = where)

  ## not accounted for and maybe not needed:  real, pairlist, double
}

.basicCoerceMethod <- function(from, to, strict = TRUE)
    stop("undefined 'coerce' method")

.makeAsMethod <- function(expr, simple, Class, ClassDef, where) {
    if(is(expr, "function")) {
        where <- environment(expr)
        args <- formalArgs(expr)
        if(!identical(args, "from"))
            expr <- .ChangeFormals(expr,
                    if(length(args) > 1) .simpleExtCoerce else .simpleIsCoerce)
        expr <- body(expr)
    }
    ## commented code below is needed if we don't assume asMethod sets the class correctly
#     if(isVirtualClass(ClassDef))
#         value <- expr
#     else if(identical(expr, quote(from)))
#         value <- substitute({class(from) <- CLASS; from},
#                            list(CLASS = Class))
#     else value <- substitute({from <- EXPR; class(from) <- CLASS; from},
#                            list(EXPR = expr, CLASS = Class) )
    ## else
    value <- expr
    if(simple && !identical(expr, quote(from)))
        value <- substitute(if(strict) EXPR else from,
                           list(EXPR = expr))
    f <- .simpleExtCoerce
    body(f, envir = where) <- value
    f
}

## check for and remove a previous coerce method.  Called from setIs
## We warn if the previous method seems to be from a
## setAs(), indicating a conflicting setIs() A previous
## version of setIs is OK, but we remove the methods anyway to be safe.
## Definitions are only removed from the current function's environment,
## not from a permanent copy.
.removePreviousCoerce <- function(from, to, where, prevIs) {
    sig <- c(from, to)
    cdef <- getGeneric("coerce", where = where)
    if(is.null(cdef))
        return(FALSE) # only for booting the methods package?
    prevCoerce <- !is.null(selectMethod("coerce", sig, TRUE, FALSE,
                                        fdef = cdef))
    rdef <- getGeneric("coerce<-", where = where)
    if(is.null(rdef))
        return(FALSE) # only for booting the methods package?
    prevRepl <- !is.null(selectMethod("coerce<-", sig, TRUE, FALSE,
                                      fdef = rdef))
    if(prevCoerce || prevRepl) {
        if(!prevIs)
            warning(gettextf("methods currently exist for coercing from %s to %s; they will be replaced.",
                             dQuote(from),
                             dQuote(to)),
                    domain = NA)
        if(prevCoerce)
            setMethod(cdef, sig, NULL, where = baseenv())
        if(prevRepl)
            setMethod(rdef, sig, NULL, where = baseenv())
        TRUE
    }
    else
        FALSE

}

canCoerce <- function(object, Class) {
    is(object, Class) ||
    !is.null(selectMethod("coerce", c(class(object), Class),
			  optional = TRUE,
			  useInherited = c(from=TRUE, to=FALSE)))
}

## turn raw function into method for coerce() or coerce<-()
## Cheats a little to get past booting the methods package
## (mainly in knowing the slots of the "signature" class).
.asCoerceMethod <- function(def, thisClass, ClassDef, replace, where) {
    fdef <-
	if(replace) quote(function(from, to = TO, value) NULL)
	else	    quote(function(from, to = TO, strict = TRUE) NULL)
    fdef[[2L]]$to <- ClassDef@className
    fdef <- eval(fdef)
    body(fdef, environment(def)) <- body(def)
    attr(fdef, "srcref") <- attr(def, "srcref")
    sig <- new("signature")
    sig@.Data <- c(thisClass, ClassDef@className)
    sig@names <- c("from", "to")
    thisPackage <- packageSlot(thisClass)
    sig@package <- if(is.null(thisPackage))
        c(getPackageName(where, FALSE), ClassDef@package) else
        c(thisPackage, ClassDef@package)
    value <- new("MethodDefinition")
    value@.Data <- fdef
    value@target <- sig
    value@defined <- sig
    value@generic <- structure( #FIXME: there should be a genericName()
                if(replace) "coerce<-" else "coerce", package = "methods")
    value
}
