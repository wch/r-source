.InitMethodsListClass <-
  function(envir)
{
    if(exists(classMetaName("MethodsList"), envir))
        return(FALSE)

    setClass("OptionalMethods", where = envir)
    setIs("function", "OptionalMethods")
    setIs("NULL", "OptionalMethods")
    setClass("MethodsList", representation(methods = "list", argument = "name", allMethods = "list"),
             prototype = list(methods=list(),  argument = as.name("<UNDEFINED>"),  allMethods = list())
             , where = envir)
    setIs("MethodsList", "OptionalMethods", where = envir)
    setClass("EmptyMethodsList", representation(argument = "name", sublist = "list"),
             where = envir)

    setClass("LinearMethodsList", representation(methods = "list", arguments = "list",
                                                 classes = "list", fromClasses = "list"),
             where = envir)
    ## the classes for method definitions
    setClass("PossibleMethod", where = envir)
    ## functions (esp. primitives) are methods
    setIs("function", "PossibleMethod", where = envir)

    ## signatures -- used mainly as named character vectors
    setClass("signature", representation("character", names = "character"), where = envir)
    
    ## formal method definition for all but primitives
    setClass("MethodDefinition", representation("function", "PossibleMethod",
                                                target = "signature", defined = "signature"), where = envir)
    setClass("MethodWithNext",
             representation("MethodDefinition", nextMethod = "PossibleMethod", excluded = "list"), where = envir)

    TRUE
}

## some intiializations that need to be done late
.InitMethodDefinitions <- function(envir) {
    assign("asMethodDefinition",  function(def, signature = list()) {
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
                  method
              }, where = envir)
    setMethod("loadMethod", "MethodWithNext",
              function(method, fname, envir) {
                  callNextMethod()
                  assign(".nextMethod", method@nextMethod, envir = envir)
                  method
              }, where = envir)
    setGeneric("findNextMethod", function(method, f = "<unknown>", mlist, optional = FALSE)
               standardGeneric("findNextMethod"), where = envir)
    setMethod("findNextMethod", "MethodDefinition",
              function(method, f, mlist, optional) {
                  value <- .findNextMethod(method, f, mlist, optional, method@defined)
                  new("MethodWithNext", method, nextMethod = value,
                      excluded = list(method@defined))
              }, where = envir)
    setMethod("findNextMethod", "MethodWithNext",
              function(method, f, mlist, optional) {
                  excluded <- c(method@excluded, list(method@defined))
                  value <- .findNextMethod(method, f, mlist, optional, excluded)
                  new("MethodWithNext", method, nextMethod = value,
                      excluded = excluded)
              }, where = envir)
    if(!isGeneric("initialize")) {
        setGeneric("initialize",  function(object, ...) {
            value <- standardGeneric("initialize")
            if(!identical(class(value), class(object)))
                stop(paste("Initialize method returned an object of class \"",
                           class(value), "\" instead of the required class \"",
                           class(object), "\"", sep=""))
            value
        }, where = envir, myDispatch = TRUE, useAsDefault = TRUE)
    }
    .InitTraceFunctions(envir)
    setMethod("initialize", "signature",
              function(object, functionDef, ...) {
                  if(nargs() < 2)
                      object
                  else if(missing(functionDef))
                      .MakeSignature(object, , list(...))
                  else if(!is(functionDef, "function"))
                      .MakeSignature(object, , list(functionDef, ...))
                  else
                      .MakeSignature(object, functionDef, list(...))
              }, where = envir)
    setMethod("initialize", "environment",
              function(object, ...) {
                  value <- new.env()
                  args <- list(...)
                  objs <- names(args)
                  for(what in objs)
                      assign(what, elNamed(args, what), envir = value)
                  value
              }, where = envir)
### Uncomment next line if we want special initialize methods for basic classes
###    .InitBasicClassMethods(where)
}

.MakeSignature <- function(object, def, signature) {
    signature <- unlist(signature)
    if(length(signature)>0) {
        classes <- as.character(signature)
        sigArgs <- names(signature)
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

        
       
