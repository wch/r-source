.InitBasicClasses <-
  function(envir)
{
    ## setClass won't allow redefining basic classes,
    ## so make the list of these empty for now.
    assign(".BasicClasses", character(), envir)
    setClass("VIRTUAL", where = envir)
    setClass("ANY", where = envir)

    setClass("vector", where = envir)
    clList <- c("logical", "numeric", "character",
                "complex", "integer", "single",
                "expression", "list")
    assign(".BasicVectorClasses",  clList, envir)
    for(.class in clList) {
        setClass(.class, prototype = newBasic(.class), where = envir)
        setIs(.class, "vector")
    }
    setIs("double", "numeric")
    setIs("integer", "numeric")
    setClass("function", prototype = function()NULL, where = envir)
    setClass("name", prototype = newBasic("name"), where = envir)

    setClass("language", where = envir)
    setClass("call", "language", prototype = quote("<undef>"()), where = envir)
    setClass("{", "language", prototype = quote({}), where = envir)
    setClass("if", "language", prototype = quote(if(NA) TRUE else FALSE), where = envir)
    setClass("<-", "language", prototype = quote("<undef>"<-NULL), where = envir)
    setSClass("environment", generatorFunction = new.env, where = envir)

    ## define some basic classes even though they aren't yet formally defined.
    ## new() will work because these classes are handled by newBasic and included
    ## in .BasicClasses

    setClass("NULL", where = envir)
    setClass("matrix", where = envir)
    setClass("array", where = envir)
    setClass("ts", where = envir)
    clList <- c(clList,
                "double", "language", "{", "if", "<-",
                "function", "environment", "named"," array",
                "matrix", "name", "call", "NULL" ,
                "VIRTUAL", "ANY", "vector", "structure")
    assign(".BasicClasses", clList, envir)
}
