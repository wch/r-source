.InitMethodsListClass <-
  function(envir)
{
    if(exists(classMetaName("MethodsList"), envir))
        return(FALSE)

    setClass("OptionalMethods", where = envir)
    setIs("function", "OptionalMethods")
    setIs("NULL", "OptionalMethods")
    setClass("MethodsList", representation(methods = "list", argument = "name", allMethods = "list", fromClass = "character"),
             prototype = list(methods=list(),  argument = as.name("<UNDEFINED>"),  allMethods = list(), fromClass = character())
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
    ## TO TRY:  add "name" as a formal slot?
    setClass("signature", "character", where = envir)
    
    ## formal method definition for all but primitives
    setClass("MethodDefinition", representation("function", "PossibleMethod",
                                                target = "signature", defined = "signature"), where = envir)
    setClass("MethodWithNext",
             representation("MethodDefinition", nextMethod = "PossibleMethod", excluded = "list"), where = envir)

    TRUE
}

## some intiializations that need to be done late
.InitMethodDefinition <- function(envir) {
    assign("asMethodDefinition",  function(def, signature = list(), argNames = character()) {
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
    classes <- as.character(signature)
    sigArgs <- names(signature)
    formalNames <- argNames[seq(length = length(classes))]
    if(length(sigArgs)< length(signature))
        names(classes) <- formalNames
    else if(length(sigArgs) > 0 && !identical(sigArgs, formalNames))
        warning("names in signature (",
                paste(sigArgs, collapse = ", "), ") don't match supplied argNames (",
                paste(formalNames, collapse = ", "),")")
    classes <- new("signature",  classes)
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
}
