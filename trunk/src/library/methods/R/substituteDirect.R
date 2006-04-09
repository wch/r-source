substituteDirect <-
  ## subsitute the for the variables named in the second argument the corresponding
  ## objects, substituting into `object'.
  ##
  ## This function differs from the ordinary `substitute' in that it treats its first argument
  ## in the standard S way, by evaluating it.  In contrast, `substitute' does
  ## not evaluate its first argument.
  function(object, frame = parent.frame(), cleanFunction = TRUE)
{
    value <- .Call("do_substitute_direct", object, frame,
                   PACKAGE = "methods")
     if(cleanFunction && is.function(value)) {
       ## unset any local environment
       environment(value) <- .GlobalEnv
     }
    value
  }

