.InitBasicClasses <-
  function(envir)
{
    ## setClass won't allow redefining basic classes,
    ## so make the list of these empty for now.
    assign(".BasicClasses", character(), envir)
    assign(".OldClasses", character(), envir)
    ## hide some functions that would break because the basic
    ## classes are not yet defined
    real.reconcileP <- reconcilePropertiesAndPrototype
    assign("reconcilePropertiesAndPrototype", function(name, properties, prototype, extends) {
        list(properties=properties, prototype = prototype, extends = extends)
    }, envir)
    setClass("VIRTUAL", where = envir)
    setClass("ANY", where = envir)
    setClass("vector", where = envir)
    setClass("missing", where = envir)
    vClasses <- c("logical", "numeric", "character",
                "complex", "integer", "single", "double",
                "expression", "list")
    for(.class in vClasses) {
        setClass(.class, prototype = newBasic(.class), where = envir)
        setIs(.class, "vector")
    }
    clList <- c(vClasses, "VIRTUAL", "ANY", "vector", "missing")
    setIs("double", "numeric")
    setIs("integer", "numeric")
    nullF <- function()NULL; environment(nullF) <- .GlobalEnv
    setClass("function", prototype = nullF, where = envir); clList <- c(clList, "function")

    setClass("language", where = envir); clList <- c(clList, "language")
    setClass("name", "language", prototype = as.name("<UNDEFINED>"), where = envir); clList <- c(clList, "name")
    setClass("call", "language", prototype = quote("<undef>"()), where = envir); clList <- c(clList, "call")
    setClass("{", "language", prototype = quote({}), where = envir); clList <- c(clList, "{")
    setClass("if", "language", prototype = quote(if(NA) TRUE else FALSE), where = envir); clList <- c(clList, "if")
    setClass("<-", "language", prototype = quote("<undef>"<-NULL), where = envir); clList <- c(clList, "<-")
    setClass("for", "language", prototype = quote(for(NAME in logical()) NULL), where = envir); clList <- c(clList, "for") 
    setClass("while", "language", prototype = quote(while(FALSE) NULL), where = envir); clList <- c(clList, "while") 
    setClass("repeat", "language", prototype = quote(repeat{break}), where = envir); clList <- c(clList, "repeat") 
    setClass("(", "language", prototype = quote((NULL)), where = envir); clList <- c(clList, "(") 
    setClass("environment", prototype = new.env(), where = envir); clList <- c(clList, "environment")

    ## define some basic classes even though they aren't yet formally defined.
    ## new() will work because these classes are handled by newBasic and included
    ## in .BasicClasses

    setClass("NULL", where = envir); clList <- c(clList, "NULL")
    setVirtual(getClassDef("NULL", where = envir), FALSE)
    
    setClass("structure", where = envir); clList <- c(clList, "structure")
    stClasses <- c("matrix", "array", "ts")
    for(.class in stClasses) {
        setClass(.class, prototype = newBasic(.class), where = envir)
        setIs(.class, "structure")
    }
    clList <- c(clList, stClasses)
    setIs("structure", "vector", coerce = function(object) as.vector(object))
    
    setIs("matrix", "array")
    setIs("array", "matrix", test = function(object) length(dim(object)) == 2)

    assign(".BasicClasses", clList, envir)

    ## some heuristics to find known old-style classes by looking for plausible
    ## method names (!)  We can't guarantee anything about this list, but it's used
    ## to avoid annoying warning message from matchSignature
    clList <- unique(c(
           substring(objects("package:base", pat = "^plot[.]"), 6),
           substring(objects("package:base", pat = "^summary[.]"), 9),
           substring(objects("package:base", pat = "^predict[.]"), 9),
           substring(objects("package:base", pat = "^Ops[.]"), 5),
           substring(objects("package:base", pat = "^print[.]"), 7)))
    ## there are no known old classes with >1 dot in the name??
    clList <- clList[-grep("[.].*[.]", clList)]
    assign(".OldClasses", clList, envir)
    ## restore the true definition of the hidden functions
    assign("reconcilePropertiesAndPrototype", real.reconcileP, envir)
}
