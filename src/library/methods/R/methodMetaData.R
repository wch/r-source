## S language code to store and retrieve the method lists for generic functions
## during a session.  Replaced by C code in method_meta_data.c (which is initialized from
## First.lib)
## 
## SessionMethodMetaData <-
##   ## An environment in which generic functions are stored during the session.
##   ## (Should only be used through the functions documented below.)
##   ##
##   ## Storing the objects there allows inherited methods to be inserted explicitly,
##   ## avoiding the cost of looking them up each time.  These assignments should not
##   ## however, go to the permanent storage, since they can change with changes in
##   ## methods or classes.
##   ##
##   ## Setting a method for a generic removes the previous version of the generic
##   ## from the session metadata.
##   "__MethodMetaData"
## assign(SessionMethodMetaData, new.env(), envir = environment())
## cat("Assigning \"__MethodMetaData\" to environment:\n");
## print(environment())
## 
## getFromMethodMetaData <-
##   # get the version of this object stored on the meta-data for methods.
##   # Returns `NULL', rather than an error, if the object is not found.
##   substitute(function(name) {
##     if(exists(name, envir = NAME, inherits=F))
##       get(name, env = NAME)
##     else
##       NULL
##   }, list(NAME=as.name(SessionMethodMetaData)))
## mode(getFromMethodMetaData) <- "function"
## 
## assignToMethodMetaData <-
##   ## assign to the session metadata for methods.
##   substitute(function(name, value)
##              assign(name, value, envir = NAME), list(NAME=as.name(SessionMethodMetaData)))
## mode(assignToMethodMetaData) <- "function"
## 
## removeFromMethodMetaData <-
##   ## remove from the session metadata for methods.
##   substitute(function(name) rm(list=name, envir=NAME), list(NAME=as.name(SessionMethodMetaData)))
## mode(removeFromMethodMetaData) <- "function"

getFromMethodMetaData <-
  function(name)
  .Call("R_get_from_method_metadata", name)

assignToMethodMetaData <-
  function(name, value)
  .Call("R_assign_to_method_metadata", name, value)

removeFromMethodMetaData <-
  function(name)
  .Call("R_remove_from_method_metadata", name)

