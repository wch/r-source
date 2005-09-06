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

    setClass("LinearMethodsList", representation(methods = "list", arguments = "list",
                                                 classes = "list", fromClasses = "list"),
             where = envir); clList <- c(clList, "LinearMethodsList")
    ## the classes for method definitions
    setClass("PossibleMethod", where = envir); clList <- c(clList, "PossibleMethod")
    ## functions (esp. primitives) are methods
    setIs("function", "PossibleMethod", where = envir)

    ## signatures -- used mainly as named character vectors
    setClass("signature", representation("character", names = "character"), where = envir); clList <- c(clList, "signature")

    ## formal method definition for all but primitives
    setClass("MethodDefinition",
             representation("function", "PossibleMethod",
                            target = "signature", defined = "signature"),
             where = envir); clList <- c(clList, "MethodDefinition")
    ## class for default methods made from ordinary functions
    setClass("derivedDefaultMethod", "MethodDefinition")
    ## class for methods with precomputed information for callNextMethod
    setClass("MethodWithNext",
             representation("MethodDefinition", nextMethod = "PossibleMethod", excluded = "list"), where = envir); clList <- c(clList, "MethodWithNext")
    setClass("SealedMethodDefinition", contains = "MethodDefinition"); clList <- c(clList, "SealedMethodDefinition")
    setClass("genericFunction",
             representation("function", generic = "character", package = "character",
                            group = "list", valueClass = "character",
                            signature = "character", default = "MethodsList",
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
    setClass("ObjectsWithPackage", representation("character", package = "character"),
             where = envir); clList <- c(clList, "ObjectsWithPackage")
    assign(".SealedClasses", c(get(".SealedClasses", envir), clList), envir);
    TRUE
}

## some initializations that need to be done late
.InitMethodDefinitions <- function(envir) {
    assign("asMethodDefinition",
           function(def, signature = list(), sealed = FALSE) {
        ## primitives can't take slots, but they are only legal as default methods
        ## and the code will just have to accomodate them in that role, w/o the
        ## MethodDefinition information.
        ## NULL is a valid def, used to remove methods.
        switch(typeof(def),
               "builtin" = , "special" = , "NULL" = return(def),
               "closure" = {},
               stop(gettextf("invalid object for formal method definition: type \"%s\"",
                             typeof(def)), domain = NA)
               )
        if(is(def, "MethodDefinition"))
            value <- def
        else
            value <- new("MethodDefinition", def)
        if(sealed)
            value <- new("SealedMethodDefinition", value)
        ## this is really new("signature",  def, signature)
        ## but bootstrapping problems force us to make
        ## the initialize method explicit here
        classes <- .MakeSignature(new("signature"),  def, signature)
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
    setGeneric("addNextMethod", function(method, f = "<unknown>", mlist, optional = FALSE, envir)
               standardGeneric("addNextMethod"), where = envir)
    setMethod("addNextMethod", "MethodDefinition",
              function(method, f, mlist, optional, envir) {
                  value <- .findNextMethod(method, f, mlist, optional, list(method@defined), envir)
                  new("MethodWithNext", method, nextMethod = value,
                      excluded = list(method@defined))
              }, where = envir)
    setMethod("addNextMethod", "MethodWithNext",
              function(method, f, mlist, optional, envir) {
                  excluded <- c(method@excluded, list(method@defined))
                  value <- .findNextMethod(method, f, mlist, optional, excluded, envir)
                  new("MethodWithNext", method, nextMethod = value,
                      excluded = excluded)
              }, where = envir)
    .initGeneric <- function(.Object, ...) {
            value <- standardGeneric("initialize")
            if(!identical(class(value), class(.Object))) {
                cv <- class(value)
                co <- class(.Object)
                if(.identC(cv, co) && is.null(packageSlot(cv))) {
                    if(is.na(match(cv, .BasicClasses))) {
                        warning(gettextf("missing package slot (%s) in object of class \"%s\" (package info added)", packageSlot(co), class(.Object)),
                                domain = NA)
                        class(value) <- class(.Object)
                    }
                    else
                        return(value)
                }
                else
                    stop(gettextf("initialize method returned an object of class \"%s\" instead of the required class \"%s\"",
                                  class(value), class(.Object)), domain = NA)
            }
            value
        }
    if(!isGeneric("initialize", envir)) {
        setGeneric("initialize",  .initGeneric, where = envir, useAsDefault = TRUE)
    }
    .InitTraceFunctions(envir)
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
    setMethod("initialize", "environment",
              function(.Object, ...) {
                  value <- new.env()
                  args <- list(...)
                  objs <- names(args)
                  for(what in objs)
                      assign(what, elNamed(args, what), envir = value)
                  value
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
		      dimnames(mat) <- list(c("Object:", "From:"),
					    rep("", length(data)))
                      show(mat)
                  }
              }, where = envir)

    setGeneric("cbind2", function(x, y) standardGeneric("cbind2"),
	       where = envir)
    ## and its default methods:
    setMethod("cbind2", signature(x = "ANY", y = "ANY"),
	      function(x,y) .Internal(cbind(deparse.level = 0, x,y)))
    setMethod("cbind2", signature(x = "ANY", y = "missing"),
	      function(x,y) .Internal(cbind(deparse.level = 0, x)))

### Uncomment next line if we want special initialize methods for basic classes
###    .InitBasicClassMethods(where)
}

.MakeSignature <- function(object, def, signature) {
    signature <- unlist(signature)
    if(length(signature)>0) {
        classes <- as.character(signature)
        sigArgs <- names(signature)
        if(is(def, "genericFunction"))
            formalNames <- def@signature
        else if(is(def, "function")) {
            formalNames <- formalArgs(def)
            dots <- match("...", formalNames)
            if(!is.na(dots))
                formalNames <- formalNames[-dots]
        }
        if(is.null(sigArgs))
            names(signature) <- formalNames[seq(along = classes)]
        else if(length(sigArgs) > 0 && any(is.na(match(sigArgs, formalNames))))
            stop(gettextf("the names in signature for method (%s) do not match function's arguments (%s)",
                          paste(sigArgs, collapse = ", "),
                          paste(formalNames, collapse = ", ")),
                 domain = NA)
        ## the named classes become the signature object
        class(signature) <- class(object)
        signature
    }
    else
        object
}
