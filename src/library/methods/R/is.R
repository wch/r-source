is <-
  # With two arguments, tests whether `object' can be treated as from `class2'.
  #
  # With one argument, returns all the super-classes of this object's class.
function(object, class2)
{
    cl <- data.class(object)
    if(missing(class2))
        return(extends(cl))
    if(identical(cl, class2) || identical(class2, "ANY"))
        return(TRUE)
    ext <- findExtends(cl, class2)
    if(is.logical(ext))
        return(ext)
    if(is.character(ext)) # by case
        return(is(object, ext))
    if(!is.list(ext))
        stop(paste("Invalid extends structure found for \"", class2,
                   "\" in examining class \"", cl, "\"", sep=""))
    f <- ext$test
    if(is.function(f))
        f(object)
    else TRUE
}

extends <-
  ## Does the first class extend the second class?
  ## Returns `maybe' if the extension includes a test.
  function(class1, class2, maybe = TRUE)
{
    if(missing(class2)) {
        if(!isClass(class1))
            return(class1)
        ext <- getExtends(getClass(class1))
        if(maybe)
            return(c(class1, names(ext)))
        else {
            tested <- sapply(ext, function(obj)(is.list(obj) && is.function(obj$test)))
            return(c(class1, names(ext[!tested])))
        }
    }
    if(identical(class1, class2))
        return(TRUE)
    value <- findExtends(class1, class2)
    if(is.logical(value))
        value
    else if(is.character(value))
        extends(value, class2)
    else if(is.list(value) && is.function(value$test))
        maybe
    else
        TRUE
}



setIs <-
  ## Defines class1 to be an extension of class2.
  ## The relationship can be conditional, if a function is supplied as the `test'
  ## argument.  If a function is supplied as the `coerce' argument, this function will
  ## be applied to any `class1' object in order to turn it into a `class2' object.
  ##
  ## Extension may imply that a `class1' object contains a `class2' object.  The default
  ## sense of containing is that all the slots of the simpler class are found in the
  ## more elaborate one.  If the `replace' argument is supplied as an S replacement
  ## function, this function will be used to implement `as(obj, class2) <- value'.
  function(class1, class2, test = NULL, coerce = NULL,
           replace = NULL, by = NULL, dataPart = NULL, where = 1)
{
    obj <- list()
    obj$test <- test
    obj$coerce <- coerce
    obj$replace <- replace
    obj$by <- by
    obj$dataPart <- if(identical(dataPart, TRUE)) TRUE else NULL
    if(length(obj) == 0)
        obj <- TRUE                     # simple extension
    classDef1 <- getClassDef(class1, where)
    classDef2 <- getClassDef(class2, where)
    if(is.null(classDef1) && is.null(classDef2))
        Stop("Neither \"", class1, "\" nor \"", class2,
             "\" has a definition in database ", where,
             ": can't store the setIs information")
    if(!is.null(classDef1)) {
        ext <- getExtends(classDef1)
        oldExt <- ext
        elNamed(ext, class2) <- obj
        setExtends(classDef1, ext)
        if(identical(where, 0))
            ## used to modify class completion information only
            return(invisible(obj))
        ## check for errors, reset the class def if they occur
        on.exit(setExtends(classDef1, oldExt))
        completeClassDefinition(class1, classDef1)
        on.exit()
        resetClass(class1)
        obj <- NULL
    }
    if(!is.null(classDef2)) {
        ## insert the extension information in the subclasses:
        ## if obj is NULL, this will delete an earlier entry (which we
        ## want to do, if we have just inserted a forward link in class1).
        ext <- getSubclasses(classDef2)
        elNamed(ext, class1) <- obj
        setSubclasses(classDef2, ext)
        if(!identical(where, 0))
            resetClass(class2)
    }
    invisible(obj)
}

