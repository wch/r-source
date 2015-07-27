#  File src/library/methods/R/MethodsListClass.R
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

.InitMethodsListClass <- function(envir)
{
    if(exists(classMetaName("MethodsList"), envir))
        return(FALSE)
    clList <- character()
    setClass("MethodsList",
             representation(methods = "list", argument = "name", allMethods = "list"),
             where = envir); clList <- c(clList, "MethodsList")
    setClass("EmptyMethodsList", representation(argument = "name", sublist = "list"),
             where = envir); clList <- c(clList, "EmptyMethodsList")

    ## the classes for method definitions
    setClass("PossibleMethod", where = envir); clList <- c(clList, "PossibleMethod")
    ## functions (esp. primitives) are methods
    setIs("function", "PossibleMethod", where = envir)

    ## the default slot of a generic function can be a method, primitive or NULL
    setClass("optionalMethod", where = envir); clList <- c(clList, "optionalMethod")
    setIs("PossibleMethod", "optionalMethod", where = envir)
    setIs("NULL", "optionalMethod", where = envir)
    ## prior to 2.11.0, the default slot in generic function objects was a MethodsList or NULL
    setIs("MethodsList", "optionalMethod", where = envir) #only until MethodsList class is defunct

    ## signatures -- multiple class names w. package slot in ||
    setClass("signature", representation("character", names = "character", package = "character"), where = envir); clList <- c(clList, "signature")

    ## className -- a single class name with package
    setClass("className", contains = "character",
             representation(package = "character"))

    ## formal method definition for all but primitives
    setClass("MethodDefinition", contains = "function",
             representation(target = "signature", defined = "signature", generic = "character"),
             where = envir); clList <- c(clList, "MethodDefinition")
    ## class for default methods made from ordinary functions
    setClass("derivedDefaultMethod", "MethodDefinition")
    ## class for methods with precomputed information for callNextMethod
    setClass("MethodWithNext",
             representation("MethodDefinition", nextMethod = "PossibleMethod", excluded = "list"), where = envir); clList <- c(clList, "MethodWithNext")
    setClass("SealedMethodDefinition", contains = "MethodDefinition"); clList <- c(clList, "SealedMethodDefinition")
    setClass("genericFunction", contains = "function",
             representation( generic = "character", package = "character",
                            group = "list", valueClass = "character",
                            signature = "character", default = "optionalMethod",
                            skeleton = "call"), where = envir); clList <- c(clList, "genericFunction")
    ## standard generic function -- allows immediate dispatch
    setClass("standardGeneric",  contains = "genericFunction")
    setClass("nonstandardGeneric", # virtual class to mark special generic/group generic
             where = envir); clList <- c(clList, "nonstandardGeneric")
    setClass("nonstandardGenericFunction",
             representation("genericFunction", "nonstandardGeneric"),
             where = envir); clList <- c(clList, "nonstandardGenericFunction")
    setClass("groupGenericFunction",
             representation("genericFunction", groupMembers = "list"),
             where = envir); clList <- c(clList, "groupGenericFunction")
    setClass("nonstandardGroupGenericFunction",
             representation("groupGenericFunction", "nonstandardGeneric"),
             where = envir); clList <- c(clList, "nonstandardGroupGenericFunction")
    setClass("LinearMethodsList", representation(methods = "list", arguments = "list",
                                                 classes = "list", generic = "genericFunction"),
             where = envir); clList <- c(clList, "LinearMethodsList")
    setClass("ObjectsWithPackage", representation("character", package = "character"),
             where = envir); clList <- c(clList, "ObjectsWithPackage")
    assign(".SealedClasses", c(get(".SealedClasses", envir), clList), envir)
    TRUE
}

## some initializations that need to be done late
.InitMethodDefinitions <- function(envir) {
    assign("asMethodDefinition",
           function(def, signature = list(.anyClassName), sealed = FALSE, fdef = def) {
        ## primitives can't take slots, but they are only legal as default methods
        ## and the code will just have to accomodate them in that role, w/o the
        ## MethodDefinition information.
        ## NULL is a valid def, used to remove methods.
        switch(typeof(def),
               "builtin" = , "special" = , "NULL" = return(def),
               "closure" = {},
               stop(gettextf("invalid object for formal method definition: type %s",
                             dQuote(typeof(def))),
                    domain = NA)
               )
        if(is(def, "MethodDefinition")) {
            value <- def
            if(missing(signature))
                signature <- value@defined
        }
        else
            value <- new("MethodDefinition", def)

        if(sealed)
            value <- new("SealedMethodDefinition", value)
        if(is(signature, "signature"))
            classes <- signature
        else
            classes <- .MakeSignature(new("signature"),  def, signature, fdef)
        value@target <- classes
        value@defined <- classes
        value
    }, envir = envir)
    setGeneric("loadMethod", where = envir)
    setMethod("loadMethod", "MethodDefinition",
              function(method, fname, envir) {
                  assign(".target", method@target, envir = envir)
                  assign(".defined", method@defined, envir = envir)
                  assign(".Method", method, envir = envir)
                  method
              }, where = envir)
    setMethod("loadMethod", "MethodWithNext",
              function(method, fname, envir) {
                  callNextMethod()
                  assign(".nextMethod", method@nextMethod, envir = envir)
                  method
              }, where = envir)
    setGeneric("addNextMethod", function(method, f = "<unknown>",
                                         mlist, optional = FALSE, envir)
               standardGeneric("addNextMethod"), where = envir)
    setMethod("addNextMethod", "MethodDefinition",
	      function(method, f, mlist, optional, envir) {
		  .findNextFromTable(method, f, optional, envir)
	      }, where = envir)
    setMethod("addNextMethod", "MethodWithNext",
	      function(method, f, mlist, optional, envir) {
		  .findNextFromTable(method, f, optional, envir, method@excluded)
	      }, where = envir)

    .initGeneric <- function(.Object, ...) {
            value <- standardGeneric("initialize")
            if(!identical(class(value), class(.Object))) {
                cv <- class(value)
                co <- class(.Object)
                if(.identC(cv[[1L]], co)) {
                  ## ignore S3 with multiple classes  or basic classes
                    if(is.na(match(cv, .BasicClasses)) &&
                       length(cv) == 1L) {
                        warning(gettextf("missing package slot (%s) in object of class %s (package info added)",
                                         packageSlot(co),
                                         dQuote(class(.Object))),
                                domain = NA)
                        class(value) <- class(.Object)
                    }
                    else
                        return(value)
                }
                else
                    stop(gettextf("'initialize' method returned an object of class %s instead of the required class %s",
                                  paste(dQuote(class(value)), collapse=", "),
                                  dQuote(class(.Object))),
                         domain = NA)
            }
            value
        }
    if(!isGeneric("initialize", envir)) {
        ## save the default method
        assign(".initialize", initialize, envir)
        setGeneric("initialize",  .initGeneric, where = envir, useAsDefault = TRUE, simpleInheritanceOnly = TRUE)
    }
    setMethod("initialize", "signature",
              function(.Object, functionDef, ...) {
                  if(nargs() < 2)
                      .Object
                  else if(missing(functionDef))
                      .MakeSignature(.Object, , list(...))
                  else if(!is(functionDef, "function"))
                      .MakeSignature(.Object, , list(functionDef, ...))
                  else
                      .MakeSignature(.Object, functionDef, list(...))
              }, where = envir)
    setMethod("initialize", "environment", # only for new("environment",...); see .InitSpecialTypesAndClasses for subclasses
              function(.Object, ...) {
                  value <- new.env()
                  args <- list(...)
                  objs <- names(args)
                  for(what in objs)
                      assign(what, elNamed(args, what), envir = value)
                  value
              }, where = envir)
    ## from 2.11.0, the MethodsList classs is deprecated
    setMethod("initialize", "MethodsList", function(.Object, ...) {
        .MlistDeprecated()
        callNextMethod()
    }, where = envir)

    ## make sure body(m) <- .... leaves a method as a method
    setGeneric("body<-", where = envir)
    setMethod("body<-", "MethodDefinition", function (fun, envir, value) {
        ff <- as(fun, "function")
        body(ff, envir = envir) <- value
        fun@.Data <- ff
        fun
    }, where = envir)
    ## a show method for lists of generic functions, etc; see metaNameUndo
    if(!isGeneric("show", envir))
        setGeneric("show", where = envir, simpleInheritanceOnly = TRUE)
    setMethod("show", "ObjectsWithPackage",
              function(object) {
                  pkg <- object@package
                  data <- as(object, "character")
                  cat("An object of class \"", class(object), "\":\n", sep="")
                  if(length(unique(pkg))==1) {
                      show(data)
                      cat("(All from \"", unique(pkg), "\")\n", sep="")
                  }
                  else {
                      mat <- rbind(data, pkg)
		      dimnames(mat) <- list(c("Object:", "Package:"),
					    rep("", length(data)))
                      show(mat)
                  }
              }, where = envir)
    ## show method for reports of method selection ambiguities; see MethodsTable.R
    setMethod("show", "MethodSelectionReport", where = envir,
              function(object) {
                  nreport <- length(object@target)
                  cat(sprintf(ngettext(nreport,
                                       "Reported %d ambiguous selection out of %d for function %s\n",
                                       "Reported %d ambiguous selections out of %d for function %s\n"),
                              nreport, length(object@allSelections), object@generic))
                  target <- object@target; selected = object@selected
                  candidates <- object@candidates; note <- object@note
                  for(i in seq_len(nreport)) {
                      these <- candidates[[i]]; notei <- note[[i]]
                      these <- these[is.na(match(these, selected[[i]]))]
                      cat(gettextf(
                                   '%d: target "%s": chose "%s" (others: %s)',
                                   i,target[[i]], selected[[i]], paste0('"', these, '"', collapse =", ")))
                      if(nzchar(notei))
                          cat(gettextf("\n    Notes: %s.\n", notei))
                      else
                          cat(".\n")
                  }
                  NULL
              })
    setMethod("show", "classGeneratorFunction", where = envir,
              function(object) {
                  cat(gettextf("class generator function for class %s from package %s\n",
                               dQuote(object@className),
                               sQuote(object@package)))
                  show(as(object, "function"))
              })

    setGeneric("cbind2", function(x, y, ...) standardGeneric("cbind2"),
	       where = envir)
    ## and its default methods:
    setMethod("cbind2", signature(x = "ANY", y = "ANY"),
	      function(x,y) .__H__.cbind(deparse.level = 0, x, y) )
    setMethod("cbind2", signature(x = "ANY", y = "missing"),
	      function(x,y) .__H__.cbind(deparse.level = 0, x) )

    setGeneric("rbind2", function(x, y, ...) standardGeneric("rbind2"),
	       where = envir)
    ## and its default methods:
    setMethod("rbind2", signature(x = "ANY", y = "ANY"),
	      function(x,y) .__H__.rbind(deparse.level = 0, x, y) )
    setMethod("rbind2", signature(x = "ANY", y = "missing"),
	      function(x,y) .__H__.rbind(deparse.level = 0, x) )

    setGeneric("kronecker", where = envir)# <- unneeded?

    setMethod("kronecker", signature(X = "ANY", Y = "ANY"),
	      function(X, Y, FUN = "*", make.dimnames = FALSE, ...)
              .kronecker(X, Y, FUN = FUN, make.dimnames = make.dimnames, ...))

    .InitStructureMethods(envir)
    ## we want special initialize methods for basic classes:
    .InitBasicClassMethods(envir)
}

.InitStructureMethods <- function(where) {
    ## these methods need to be cached (for the sake of the primitive
    ## functions in the group) if a class is loaded that extends
    ## one of the classes in `needed` (other classes than "structure" now
    ## also require generics for some primitives).
    if(!exists(".NeedPrimitiveMethods", where))
      needed <- list()
    else
      needed <- get(".NeedPrimitiveMethods", where)
    needed <- c(needed, list(structure = "Ops", vector = "Ops",
          array = "Ops", nonStructure = "Ops"),
          array = "[", structure = "[", nonStructure = "[",
          structure = "Math", nonStructure = "Math",
          refClass = "$", refClass = "$<-", data.frame = "$<-"
                )
    assign(".NeedPrimitiveMethods", needed, where)
    setMethod("Ops", c("structure", "vector"), where = where,
              function(e1, e2) {
                  value <- callGeneric(e1@.Data, e2)
                  if(length(value) == length(e1)) {
                      e1@.Data <- value
                      e1
                  }
                  else
                    value
              })
    setMethod("Ops", c("vector", "structure"), where = where,
              function(e1, e2) {
                  value <- callGeneric(e1, e2@.Data)
                  if(length(value) == length(e2)) {
                      e2@.Data <- value
                      e2
                  }
                  else
                    value
              })
    setMethod("Ops", c("structure", "structure"), where = where,
              function(e1, e2)
                 callGeneric(e1@.Data, e2@.Data)
              )
    ## We need some special cases for matrix and array.
    ## Although they extend "structure", their .Data "slot" is the matrix/array
    ## So op'ing them with a structure gives the matrix/array:  Not good?
    ## Following makes them obey the structure rule.
    setMethod("Ops", c("structure", "array"), where = where,
              function(e1, e2)
                 callGeneric(e1@.Data, as.vector(e2))
              )
    setMethod("Ops", c("array", "structure"), where = where,
              function(e1, e2)
                 callGeneric(as.vector(e1), e2@.Data)
              )
    ## but for two array-based strucures, we let the underlying
    ## code for matrix/array stand.
    setMethod("Ops", c("array", "array"), where = where,
              function(e1, e2)
                 callGeneric(e1@.Data, e2@.Data)
              )


    setMethod("Math", "structure", where = where,
              function(x) {
                  x@.Data <- callGeneric(x@.Data)
                  x
              })
    setMethod("Math2", "structure", where = where,
              function(x, digits) {
                  value <- x
                  x <- x@.Data
                  value@.Data  <- callGeneric()
                  value
              })
    ## some methods for nonStructure, ensuring that the class and slots
    ## will be discarded
    setMethod("Ops", c("nonStructure", "vector"), where = where,
              function(e1, e2) {
                  callGeneric(e1@.Data, e2)
              })
    setMethod("Ops", c("vector", "nonStructure"), where = where,
              function(e1, e2) {
                  callGeneric(e1, e2@.Data)
              })
    setMethod("Ops", c("nonStructure", "nonStructure"), where = where,
              function(e1, e2)
                 callGeneric(e1@.Data, e2@.Data)
              )
    setMethod("Math", "nonStructure", where = where,
              function(x) {
                  callGeneric(x@.Data)
              })
    setMethod("Math2", "nonStructure", where = where,
              function(x, digits) {
                  x <- x@.Data
                  callGeneric()
              })
    setMethod("[", "nonStructure", where = where,
                        function (x, i, j, ..., drop = TRUE)
                        {
                          value <- callNextMethod()
                          value@.Data
                        })

}


.MakeSignature <- function(object, def = NULL, signature, fdef = def) {
    ## fill in the signature information in object
    ## In effect, object must come from class "signature" or a subclass
    ## but the only explicit requirement is that it has compatible
    ## .Data and "package" slots
    signature <- unlist(signature)
    if(length(signature)>0) {
        classes <- as.character(signature)
        sigArgs <- names(signature)
        pkgs <- attr(signature, "package")
        if(is.null(pkgs))
            pkgs <- character(length(signature))
        if(is(fdef, "genericFunction"))
            formalNames <- fdef@signature
        else if(is.function(def)) {
            if(!is(fdef, "function")) fdef <- def
            formalNames <- formalArgs(fdef)
            dots <- match("...", formalNames)
            if(!is.na(dots))
                formalNames <- formalNames[-dots]
        }
        else formalNames <- character()
        if(length(formalNames) > 0) {
            if(is.null(sigArgs))
              names(signature) <- formalNames[seq_along(classes)]
            else if(length(sigArgs) && any(is.na(match(sigArgs, formalNames))))
                if(is(fdef, "genericFunction"))
                      stop(sprintf(gettext("the names in signature for method (%s) do not match %s's arguments (%s)", domain = "R-methods"),
                            paste(sigArgs, collapse = ", "),
                            fdef@generic,
                            paste(formalNames, collapse = ", ")),
                   domain = NA)
                else
                      stop(sprintf(gettext("the names in signature for method (%s) do not match function's arguments (%s)", domain = "R-methods"),
                            paste(sigArgs, collapse = ", "),
                            paste(formalNames, collapse = ", ")),
                   domain = NA)
        }
        object@.Data <- signature
        object@package <- pkgs
    }
    object
}
