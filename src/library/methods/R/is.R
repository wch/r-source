is <-
  # With two arguments, tests whether `object' can be treated as from `class2'.
  #
  # With one argument, returns all the super-classes of this object's class.
function(object, class2)
{
    cl <- .class1(object)
    if(missing(class2))
        return(extends(cl))
    if(identical(cl, class2) || identical(class2, "ANY"))
        return(TRUE)
    ext <- possibleExtends(cl, class2)
    if(is.logical(ext))
        ext
    else if(ext@simple)
        TRUE
    else
       ext@test(object)
}

extends <-
  ## Does the first class extend the second class?
  ## Returns `maybe' if the extension includes a non-trivial test.
  function(class1, class2, maybe = TRUE)
{
    if(is(class1, "classRepresentation")) {
        classDef1 <- class1
        class1 <- getClassName(classDef1)
    }
    else if(is.character(class1) && length(class1)==1)
        classDef1 <- getClass(class1, TRUE)
    else
        stop("class1 must be the name of a class or a class definition")
    if(missing(class2)) {
        if(is.null(classDef1))
            return(class1)
        ext <- getExtends(classDef1)
        if(identical(maybe, TRUE))
            return(c(class1, names(ext)))
        else {
            noTest <- sapply(ext, function(obj)identical(obj@test, .simpleExtTest))
            return(c(class1, names(ext[noTest])))
        }
    }
    if(identical(class1, class2))
        return(TRUE)
    if(is(class2, "classRepresentation"))
        class2 <- getClassName(class2)
    else if(!(is.character(class2) && length(class2) == 1))
        stop("class2 must be the name of a class or a class definition")
    value <- possibleExtends(class1, class2)
    if(is.logical(value))
        value
    else if(value@simple || identical(value@test, .simpleExtTest))
        TRUE
    else
        maybe
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
           replace = NULL, by = character(), where = 1)
{
    ## Technical detail:  we only use the class definition for the class name & the
    ## package, and call getClassDef  (don't  complete the def'n), because setIs is called
    ## during the setClass computations to record simple contained classes &
    ## completeClassDefinition may get into a loop then.
    classDef1 <- getClassDef(class1)
    ## But, we want at least a minimal definition to allow for relations with old-style
    ## classes: so,
    if(!is(classDef1, "classRepresentation"))
        classDef1 <- getClass(class1, TRUE)
    classDef2 <- getClassDef(class2)
    if(!is(classDef2, "classRepresentation"))
        classDef2 <- getClass(class2, TRUE)
    if((is.null(classDef1) || is.null(classDef2)) &&
       !(isVirtualClass(class1) && isVirtualClass(class2)))
        stop(paste("Both \"", class1, "\" nor \"", class2,
             "\" must be defined to create an is relation between them", sep=""))
    obj <- makeExtends(class1, class2, coerce, test, replace, by,
                       classDef1 = classDef1, classDef2 = classDef2)
    setExtendsMetaData(classDef1, classDef2, obj, where = where)
    subDef <- setSubclassMetaData(classDef2, classDef1, where = where)
    resetClass(class1)
    ## Usually it would be OK to do:  resetClass(class2)
    ## However, resetting a basic class can throw us into a loop.
    classDef2 <- getClassDef(class2, 0)
    if(!is.null(classDef2)) {
        elNamed(classDef2@subclasses, class1) <- subDef
        assignClassDef(class2, classDef2, 0)
    }
    invisible(obj)
}

