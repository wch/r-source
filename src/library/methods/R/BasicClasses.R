.InitBasicClasses <-
  function(envir)
{
    setClass("VIRTUAL", where = envir)
    setClass("ANY", where = envir)

    setClass("vector", where = envir)
    for(.class in .BasicVectorClasses) {
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
}
