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
    if(class1 == class2)
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
           replace = NULL, where = -1)
{
    obj <- list()
    obj$test <- test
    obj$coerce <- coerce
    obj$replace <- replace
    if(length(obj) == 0)
        obj <- TRUE                     # simple extension
    if(isClass(class1)) {
        def <- getClassDef(class1, where = where)
        ext <- getExtends(def)
        oldExt <- ext
        elNamed(ext, class2) <- obj
        setExtends(def, ext)
        ## check for errors, reset the class def if they occur
        on.exit(setExtends(def, oldExt))
        completeClassDefinition(class1, def)
        on.exit()
        resetClass(class1)
    }
    else if(isClass(class2)) {
        ## put the information in the subclasses of class2
        ## (A bit dicey, since we can't check errors with
        ## completeClassDefinition)
        def <- getClassDef(class2, where = where)
        ext <- getSubclasses(def)
        elNamed(ext, class1) <- obj
        setSubclasses(def, ext)
        resetClass(class2)
    }
    else
        stop("Can't set an is relation unless one of the classes has a formal definition")
}

