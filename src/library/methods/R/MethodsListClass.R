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
    setClass("Method")
    ## functions (esp. primitives) are methods
    setIs("function", "Method")
    ## formal method definition for all but primitives
    setClass("MethodDefinition", representation("function", "Method",
                                                selected = "list", defined = "list"))

    TRUE
}
