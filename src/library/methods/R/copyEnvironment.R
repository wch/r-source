copyEnvironment <-
  ## Returns a new environment in which all the objects found in the object's
  ## original environment have been assigned.  The environment may be NULL, in
  ## which case `NULL' is returned.  Objects named in the exceptions argument are not copied.
  function(object, exceptions = character())
{
    if(is.environment(object))
        env <- object
    else
        env <- environment(object)
    if(is.null(env))
        return(NULL)
    value <- new.env()
    copyThese <- objects(env, all=TRUE)
    if(length(exceptions)>0)
        copyThese <- copyThese[is.na(match(copyThese, exceptions))]
    for(what in copyThese)
        assign(what, get(what, env), value)
    value
}
