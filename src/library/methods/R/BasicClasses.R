.InitBasicClasses <-
  function(envir)
{
    ## setClass won't allow redefining basic classes,
    ## so make the list of these empty for now.
    assign(".BasicClasses", character(), envir)
    setClass("VIRTUAL", where = envir)
    setClass("ANY", where = envir)
    setClass("vector", where = envir)
    vClasses <- c("logical", "numeric", "character",
                "complex", "integer", "single",
                "expression", "list")
    for(.class in vClasses) {
        setClass(.class, prototype = newBasic(.class), where = envir)
        setIs(.class, "vector")
    }
    clList <- c(vClasses, "VIRTUAL", "ANY", "vector")
    setIs("double", "numeric")
    setIs("integer", "numeric")
    setClass("function", prototype = function()NULL, where = envir); clList <- c(clList, "function")
    setClass("name", prototype = newBasic("name"), where = envir); clList <- c(clList, "name")

    setClass("language", where = envir); clList <- c(clList, "language")
    setClass("call", "language", prototype = quote("<undef>"()), where = envir); clList <- c(clList, "call")
    setClass("{", "language", prototype = quote({}), where = envir); clList <- c(clList, "{")
    setClass("if", "language", prototype = quote(if(NA) TRUE else FALSE), where = envir); clList <- c(clList, "if")
    setClass("<-", "language", prototype = quote("<undef>"<-NULL), where = envir); clList <- c(clList, "<-")
    setClass("environment", prototype = new.env(), where = envir); clList <- c(clList, "environment")

    ## define some basic classes even though they aren't yet formally defined.
    ## new() will work because these classes are handled by newBasic and included
    ## in .BasicClasses

    setClass("NULL", where = envir); clList <- c(clList, "NULL")

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
}
