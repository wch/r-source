.InitMethodsListClass <-
  function(envir)
{
    if(exists(classMetaName("MethodsList"), envir))
        return(FALSE)

    setClass("MethodsList",
             representation(methods = "list", argument = "name", allMethods = "list"),
             sealed = TRUE, where = envir)
    setClass("EmptyMethodsList", representation(argument = "name", sublist = "list"),
             sealed = TRUE, where = envir)

    setClass("LinearMethodsList", representation(methods = "list", arguments = "list",
                                                 classes = "list", fromClasses = "list"),
             sealed = TRUE, where = envir)
    ## the classes for method definitions
    setClass("PossibleMethod", sealed = TRUE, where = envir)
    ## functions (esp. primitives) are methods
    setIs("function", "PossibleMethod", where = envir)

    ## signatures -- used mainly as named character vectors
    setClass("signature", representation("character", names = "character"), sealed = TRUE, where = envir)
    
    ## formal method definition for all but primitives
    setClass("MethodDefinition",
             representation("function", "PossibleMethod",
                            target = "signature", defined = "signature"),
             sealed = TRUE, where = envir)
    setClass("MethodWithNext",
             representation("MethodDefinition", nextMethod = "PossibleMethod", excluded = "list"), sealed = TRUE, where = envir)
    setClass("SealedMethodDefinition", contains = "MethodDefinition")
    setClass("genericFunction",
             representation("function", generic = "character", package = "character",
                            group = "list", valueClass = "character",
                            signature = "character", default = "MethodsList",
                            skeleton = "call"))
    setClass("nonstandardGeneric", # virtual class to mark special generic/group generic
             sealed = TRUE, where = envir)
    setClass("nonstandardGenericFunction",
             representation("genericFunction", "nonstandardGeneric"),
             sealed = TRUE, where = envir)
    setClass("groupGenericFunction",
             representation("genericFunction", groupMembers = "list"),
             sealed = TRUE, where = envir)
    setClass("nonstandardGroupGenericFunction",
             representation("groupGenericFunction", "nonstandardGeneric"),
             sealed = TRUE, where = envir)
    setClass("ObjectsWithPackage", representation("character", package = "character"),
             sealed = TRUE, where = envir)

    TRUE
}

## some intiializations that need to be done late
.InitMethodDefinitions <- function(envir) {
    assign("asMethodDefinition",  function(def, signature = list(), sealed = FALSE) {
        ## primitives can't take slots, but they are only legal as default methods
        ## and the code will just have to accomodate them in that role, w/o the
        ## MethodDefinition information.
        ## NULL is a valid def, used to remove methods.
        switch(typeof(def),
               "builtin" = , "special" = , "NULL" = return(def),
               "closure" = {},
               stop(paste("Invalid object for formal method defintion: type \"",
                          typeof(def), "\"", sep=""))
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
                  callNextMethod(method, fname, envir)
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
    if(!isGeneric("initialize")) {
        setGeneric("initialize",  function(.Object, ...) {
            value <- standardGeneric("initialize")
            if(!identical(class(value), class(.Object)))
                stop(paste("Initialize method returned an object of class \"",
                           class(value), "\" instead of the required class \"",
                           class(.Object), "\"", sep=""))
            value
        }, where = envir, useAsDefault = TRUE)
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
    setGeneric("body<-")
    setMethod("body<-", "MethodDefinition", function (f, value, envir) {
        ff <- as(f, "function")
        body(ff, envir = envir) <- value
        f@.Data <- ff
        f
    })
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
        else if(is(def, "function"))
            formalNames <- formalArgs(def)
        if(length(sigArgs)< length(signature))
            names(signature) <- formalNames[seq(along = classes)]
        else if(length(sigArgs) > 0 && any(is.na(match(sigArgs, formalNames))))
            stop(paste("names in signature (",
                       paste(sigArgs, collapse = ", "), ") don't match function's arguments (",
                       paste(formalNames, collapse = ", "),")", sep=""))
        ## the named classes become the signature object
        class(signature) <- class(object)
        signature
    }
    else
        object
}

        
       
