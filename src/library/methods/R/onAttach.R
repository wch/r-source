cacheGenericsMetaData <-
  ## 
  function(generics, attach) {
    for(f in generics) {
      ## find the function.  It may be a generic, but will be a primitive
      ## if the internal C code is being used to dispatch methods for primitives.
      ## It may also be NULL, if no function is found (when detaching the only
      ## package defining this function, for example).
      fdef <- getFunction(f, mustFind = FALSE)
      switch(typeof(fdef),
             closure = , NULL = {
               if(!is.null(getFromMethodMetaData(f)))
                 removeFromMethodMetaData(f)
             },
             builtin = , special = {
               if(attach) code <- "modify"
               else {
                 code <- "delete"
                 for(i in search()) {
                   envi <- as.environment(i)
                   if(!identical(envi, env) &&
                      !is.null(getMethodsMetaData(f, envi)))
                     code <- "modify"
                 }
               }
               setPrimitiveMethods(f, fdef, code)
             }
             )
    }
  }
