.InitMethodsListClass <-
  function(envir) {
  if(exists(classMetaName("MethodsList"), envir))
     return(FALSE)
  
  setClass("OptionalMethods", where = envir)
  setIs("function", "OptionalMethods")
  setIs("NULL", "OptionalMethods")
  setClass("MethodsList", representation(methods = "list", argument = "name", allMethods = "list"),
           prototype = list(methods=list(),  argument = as.name("<UNDEFINED>"),  allMethods = list(), form = quote(f()))
           , where = envir)
  setIs("MethodsList", "OptionalMethods")
  
  TRUE
}
