is <-
  # With two arguments, tests whether `object' can be treated as from `class2'.
  #
  # With one argument, returns all the super-classes of this object's class.
function(object, class2)
{
    cl <- .class1(object)
    if(missing(class2))
        return(extends(cl))
    if(.identC(cl, class2) || .identC(class2, "ANY"))
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
  function(class1, class2, maybe = TRUE, fullInfo = FALSE)
{
    if(is(class1, "classRepresentation")) {
        classDef1 <- class1
        class1 <- classDef1@className
    }
    else if(is.character(class1)){
        classDef1 <- getClassDef(class1)
    }
    else
        stop("'class1' must be the name of a class or a class definition")
    if(missing(class2)) {
        if(is.null(classDef1))
            return(class1)
        ext <- classDef1@contains
        if(!identical(maybe, TRUE))
        {
            noTest <- sapply(ext, function(obj)identical(obj@test, .simpleExtTest))
            ext <- ext[noTest]
        }
        if(fullInfo) {
            elNamed(ext, class1) <- TRUE
            return(ext)
        }
        else
            return(c(class1,names(ext)))
    }
    ## the [[1]] below handles old-style classes & throws away package attributes
    ## A cleaner version needed, to also ignore attr's of class2
    if(.identC(class1[[1]], class2) || .identC(class2, "ANY"))
            return(TRUE)
    if(is(class2, "classRepresentation")){
        classDef2 <- class2
        class2 <- class2@className
    }
    else if(!(is.character(class2) && length(class2) == 1))
        stop("'class2' must be the name of a class or a class definition")
    else
        classDef2 <- getClassDef(class2)
    value <- possibleExtends(class1, class2, classDef1, classDef2)
    if(fullInfo)
        value
    else if(is.logical(value))
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
           replace = NULL, by = character(), where = topenv(parent.frame()),
           classDef = getClass(class1, TRUE, where = where), extensionObject = NULL, doComplete = TRUE)
{
    ## class2 should exist
    where <- as.environment(where)
    classDef2 <- getClassDef(class2, where)
    if(is.null(classDef2))
        stop(gettextf("class \"%s\" has no visible definition from package or environment '%s'", class2, getPackageName(where)), domain = NA)
    ## check some requirements:
    ## One of the classes must be on the target environment (so that the relation can
    ## be retained by saving the corresponding image)
    m1 <- classMetaName(class1)
    local1 <- exists(m1, where, inherits = FALSE) &&
    !(classDef@sealed || bindingIsLocked(m1, where))
    m2 <- classMetaName(class2)
    local2 <- exists(m2, where, inherits = FALSE) &&
    !(classDef2@sealed || bindingIsLocked(m2, where))
    if(!(local1 || local2) )
        stop(gettextf("cannot create a 'setIs' relation when neither of the classes (\"%s\" and \"%s\") is local and modifiable in this package",
                      class1, class2), domain = NA)
    if(classDef@sealed && !isClassUnion(classDef2))
        stop(gettextf("class \"%s\" is sealed; new superclasses can not be defined, except by 'setClassUnion'", class1), domain = NA)
    prevIs <- !identical(possibleExtends(class1, class2,classDef, classDef2),
                         FALSE) # used in checking for previous coerce
    if(is.null(extensionObject))
        obj <- makeExtends(class1, class2, coerce, test, replace, by,
                           classDef1 = classDef, classDef2 = classDef2,
                           package = getPackageName(where))
    else
        obj <- extensionObject
    ## revise the superclass/subclass info in the stored class definition
    .validExtends(class1, class2, classDef,  classDef2, obj@simple)
    if(!classDef@sealed) {
        where1 <- findClass(class1, where)[[1]]
        if(!bindingIsLocked(m1, where1)) {
            ## the direct contains information
            elNamed(classDef@contains, class2) <- obj
            if(doComplete)
                classDef@contains <- completeExtends(classDef, class2, obj, where = where)
            assignClassDef(class1, classDef, where1)
        }
    }
    if(!classDef2@sealed) {
        where2 <- findClass(class2, where)[[1]]
        if(!bindingIsLocked(m2, where2)) {
            elNamed(classDef2@subclasses, class1) <- obj
            if(doComplete)
                classDef2@subclasses <- completeSubclasses(classDef2, class1, obj, where)
            assignClassDef(class2, classDef2, where2)
        }
        .removePreviousCoerce(class1, class2, where, prevIs)
    }
    invisible(classDef)
}

.validExtends <- function(class1, class2, classDef1,  classDef2, slotTests) {
    .msg <- function(class1, class2) gettextf("class \"%s\" cannot extend class \"%s\"", class1, class2)
    if((is.null(classDef1) || is.null(classDef2)) &&
       !(isVirtualClass(class1) && isVirtualClass(class2)))
        stop(.msg(class2, class2), ": ",
             gettext("Both classes must be defined"),
             domain = NA)
    if(slotTests) {
        slots2 <- classDef2@slots
        if(length(slots2) > 0) {
            n2 <- names(slots2)
            slots1 <- classDef1@slots
            n1 <- names(slots1)
            if(any(is.na(match(n2, n1))))
                stop(.msg(class2, class2), ": ",
                     gettextf("class \"%s\" is missing slots from class \"%s\" (%s), and no coerce method was supplied",
                              class1, class2,
                              paste(n2[is.na(match(n2, n1))], collapse = ", ")),
                     domain = NA)
            bad <- character()
            for(what in n2)
                if(!extends(elNamed(slots1, what), elNamed(slots2, what)))
                    bad <- c(bad, what)
            if(length(bad)>0)
                stop(.msg(class1, class2), ": ",
                     gettextf("slots in class \"%s\" must extend corresponding slots in class \"%s\": fails for %s",
                              class1, class2, paste(bad, collapse = ", ")),
                     domain = NA)
        }
    }
    TRUE
}


