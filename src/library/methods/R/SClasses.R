setClass <-
  ## Define Class to be an S-style class.
  function(Class, representation = list(), prototype = NULL,
                     contains = character(), validity, access,
                     where = 1, version = FALSE)
{
    slots <- nchar(allNames(representation))>0
    extends <- c(as.character(representation[!slots]), contains)
    properties <- representation[slots]
    setSClass(Class, properties,extends, prototype, where=where)
}

representation <-
  ## Representation of a class; that is,
  ## a list of named slots and unnamed classes to be included in a class
  ## definition.
  function(...)
{
    value <- list(...)
    ## unlike the S-Plus function, this does not form the class representation,
    ## since set SClass works separately with the slots and extends arguments.
    anames <- allNames(value)
    for(i in seq(along=value)) {
        ei <- el(value, i)
        if(!is.character(ei) || length(ei) != 1)
            stop(paste("Element", i, "of the representation was not a single character string"))
        ## we don't warn on the non-definition of the corresponding class; basic classes
        ## can exist w/o definition, and the prototype generation can be delayed until
        ## the first call to new() on this class.
    }
    includes <- as.character(value[nchar(anames)==0])
    if(any(duplicated(includes)))
        stop(paste("Duplicate class names among superclasses:", paste(includes[duplicated(includes)], collapse = ", ")))
    value
}

setSClass <-
  ## Set the Class Definition.
  ## The formal definition of the class is set according to the arguments.
  ##
  ## This function is not S-Plus compatible unless the OOP library is included in
  ## the S-Plus run.  For greater compatibility, use `setClass' instead.
  function(name, properties = list(), extends = character(), prototype = NULL, generatorFunction, where = 1, subclasses = character(), virtual = NA)
{
    ## remove from the cached definitions (only) right away
    removeClass(name, where = 0)
    extends <- makeExtends(extends)
    subclasses <- makeExtends(subclasses)
    if(!is.null(prototype) || length(properties)>0) {
        if(is.null(prototype))
            prototype <- list()         ## something we can set slots in
        ## make the prototype look like a "legal" element of the class
        prototype <- reconcilePropertiesAndPrototype(name, properties, prototype)
    }
    ev <- newClassEnvironment(name, properties, extends, prototype, subclasses, virtual)
    if(missing(generatorFunction)) {
        f <- quote(function(...)new(Class=, ...))
    }
    else {
        assign(".Generator", generatorFunction, ev)
        f <- quote(function(...).Generator(...))
    }
    mode(f) <- "function"               ## because mode is call otherwise
    class(f) <- "classRepEnvironment"
    environment(f) <- ev
    assignClassDef(name, f, where)
    name
}

getClassDef <-
  ## Get the definition of the class supplied as a string.
  ## This is the unexpanded definition.  If you want all the information currently
  ## known, such as the complete list of superclasses, use `getClass(Class)'.
  function(Class, where = -1)
{
    get(classMetaName(Class), where)
}

getClass <-
  ## Get the complete definition of the class supplied as a string,
  ## including all slots, etc. in classes that this class extends.
  function(Class)
{
    cname <- classMetaName(Class)
    value <- getFromClassMetaData(cname)
    if(is.null(value))
        value <- completeClassDefinition(Class)
    value
}

slot <-
  ## Get the value of the named slot.  This function does exact, not partial, matching of names,
  ## and the name must be one of the slot names specified in the class's definition.
  ##
  ## Because slots are stored as attributes, the validity check is not 100% guaranteed,
  ## but should be OK if nobody has "cheated" (e.g., by setting other attributes directly).
  function(object, name)
    .Call("R_get_slot", object, name)

"slot<-" <-
  ## Set the value of the named slot.  Must be one of the slots in the class's definition.
  function(object, name, check = T, value)
    .Call("R_set_slot", object, name, check, value)

## "@" <-
##   function(object, name)
##   .Internal(object@name)

## "@<-" <-
##   function(object, name, value)
##   .Internal(object@name <- value)

slotNames <-
  ##  The names of the class's slots.  The argument is either the name of a class, or
  ## an object from the relevant class.
  function(x)
{
    if(is.character(x) && length(x) == 1)
        Class <- x
    else
        Class <- data.class(x)
    names(getProperties(getClass(Class)))
}

removeClass <-
  ## Remove the definition of this class.
  function(Class, where = -1)
{
    ## always remove the cached version, if any
    Class <- classMetaName(Class)
    if(!is.null(getFromClassMetaData(Class)))
        removeFromClassMetaData(Class)
    if(identical(where, -1))
        rm(list=Class, pos=.GlobalEnv)
    else if(!identical(where, 0))
        rm(list=Class, pos=where)
}


isClass <-
  ## Is this a formally defined class?
  function(what, formal=TRUE)
{
    ## argument formal is for Splus compatibility & is ignored.  (All classes that
    ## are defined must have a class definition object.)
    exists(classMetaName(what))
}

new <-
  ## Generate an object from the specified class.
  ##
  ## If other arguments are included, these are the values for named properties
  ## in the object, or an object that can be coerced into this class.
  ## The special argument `.Force' controls what happens if the class is undefined: if
  ## it is `TRUE' an empty object is returned (essentially a `NULL', except that R currently
  ## doesn't allow `NULL' in certain contexts, so this object has a class as well).
  ## If `.Force' is `FALSE', trying to create an object from an undefined class is an
  ## error.  Note that the basic vector classes, `"numeric"', etc. are implicitly defined,
  ## so one can use `new' for these classes.
  ###
  ### Unnamed arguments are objects from this class or a superclass.
  ##
  ## When called with no Class argument, assumes the call comes from the generating function
  ## for a class, and uses that function's environment as the class representation.
  function(Class, ..., .Force=FALSE)
{
    if(!isClass(Class))
        return(newBasic(Class, ..., .Force = .Force))
    ## get the class definition, completing it if this is the first reference
    ## to this class in this session.
    ClassDef <- getClass(Class)
    if(identical(getVirtual(ClassDef), TRUE)) {
        if(.Force)
            value <- newEmptyObject()
        else
            stop("Trying to use new() on a virtual class")
    }
    else
        value <- getPrototype(ClassDef)
    class(value) <- Class
    args <- list(...)
    if(length(args) > 0) {
        ## separate the slots, superclass objects
        snames <- allNames(args)
        which <- nchar(snames)>0
        elements <- args[which]
        supers <- args[!which]
        if(length(supers) > 0) {
            for(i in seq(along = supers)) {
                obj <- el(supers, i)
                Classi <- data.class(obj)
                if(identical(Classi, Class))
                    value <- obj
                else if(extends(Classi, Class))
                    ## principally the identical case:
                    value <- as(obj, Class)
                else if(extends(Class, Classi))
                    as(value, Classi) <- obj
                else
                    stop(paste("Can't use object of class \"", Classi,Def,
                               "\" in new():  Class \"", Class, "\" does not extend that class"))
            }
        }
        if(length(elements)>0) {
            snames <- names(elements)
            which  <- match(snames, slotNames(value))
            if(any(is.na(which)))
                stop(paste("Invalid names for properties of class ",
                           Class, ": ", paste(snames[is.na(which)], collapse=", ")))
            for(i in seq(along=snames))
                slot(value, el(snames, i)) <- el(args, i)
        }
    }
    value
}

getClasses <-
  ## The names of all the classes formally defined on `where'.
  ## If called with no argument, all the classes currently known in the session
  ## (which does not include classes that may be defined on one of the attached
  ## libraries, but have not yet been used in the session).
  function(where = get(SessionClassMetaData))
{
    pat <- paste("^",classMetaName(""), sep="")
    names <- objects(where, pattern = pat, all.names = TRUE)
    ## strip off the leading pattern (this implicitly assumes the characters
    ## in classMetaName("") are either "." or not metacharacters
    substring(names, nchar(pat))
}

unClass <-
  ## returns the object containing the values of all the slots in this object's
  ## class definition (specifically, ithe returned object has attributes corresponding
  ## to each slot), in the case that the object's class is formally defined, with slots.
  ##
  ## The semantics of this function in R are slightly different from the `unclass' function in S-Plus,
  ##  which produces either a list of the slots or an object with the class of this class's
  ## prototype, when
  ## the class is not defined with slots.
  function(x)
{
    Class <- data.class(x)
    what <- slotNames(Class)
    if(length(what) > 0) {
        attributes(x)[what]
    }
    else {
        class(x) <- NULL
        x
    }
}
