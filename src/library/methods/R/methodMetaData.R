
getFromMethodMetaData <-
  function(name)
  .Call("R_get_from_method_metadata", name, PACKAGE = "methods")

assignToMethodMetaData <-
  function(name, value)
  .Call("R_assign_to_method_metadata", name, value, PACKAGE = "methods")


removeFromMethodMetaData <-
  function(name)
  .Call("R_remove_from_method_metadata", name, PACKAGE = "methods")

