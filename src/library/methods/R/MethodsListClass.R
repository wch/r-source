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
    setValidity("MethodsList", function(object) {
      if(length(object@allMethods) != length(object@fromClass))
        "Incompatible slots allMethods and fromClass (lengths not equal)"
      else
        TRUE
    })
    setIs("MethodsList", "OptionalMethods", where = envir)
    setClass("EmptyMethodsList", representation(argument = "name", sublist = "list"),
             where = envir)

    setClass("LinearMethodsList", representation(methods = "list", arguments = "list",
                                                 classes = "list", fromClasses = "list"),
             where = envir)

    TRUE
}
