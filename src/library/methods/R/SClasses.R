setClass <-
  ## Define Class to be an S-style class.
  function(Class, representation = list(), prototype = NULL,
                     contains = character(), validity = NULL, access = NULL,
                     where = 1, version = FALSE)
{
    if(!is.na(match(Class, .BasicClasses)))
        stop(paste("\"", Class, "\" is a basic class and cannot be redefined", sep=""))
    slots <- nchar(allNames(representation))>0
    extends <- c(as.character(representation[!slots]), contains)
    properties <- representation[slots]
    setSClass(Class, properties,extends, prototype, where=where,
              validity = validity, access = access)
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
    }
    includes <- as.character(value[nchar(anames)==0])
    if(any(duplicated(includes)))
        stop(paste("Duplicate class names among superclasses:", paste(includes[duplicated(includes)], collapse = ", ")))
    slots <- anames[nchar(anames)>0]
    if(any(duplicated(slots)))
       stop(paste("Duplicated slot names: ", paste(slots[duplicated(slots)], collapse="")))
    value
}

setSClass <-
  ## Set the Class Definition.
  ## The formal definition of the class is set according to the arguments.
  ##
  ## Users should call setClass instead of this function.
  function(name, properties = list(), extends = character(), prototype = NULL, generatorFunction, where = 1, subclasses = character(), virtual = NA, validity = NULL, access = NULL)
{
    ## remove from the cached definition (only) right away
    removeClass(name, where = 0)
    extends <- makeExtends(extends)
    subclasses <- makeExtends(subclasses)
    if(!is.null(prototype) || length(properties)>0) {
        ## make the prototype look like a "legal" element of the class
        prototype <- reconcilePropertiesAndPrototype(name, properties, prototype, extends)
    }
    ev <- newClassEnvironment(name, properties, extends, prototype, subclasses,
                              virtual, validity, access)
    if(missing(generatorFunction)) {
        f <- substitute(function(...)newObject(Class=CLASS, ...), list(CLASS=name))
    }
    else {
        assign(".Generator", generatorFunction, ev)
        f <- quote(function(...).Generator(...))
    }
    mode(f) <- "function"               ## because mode is call otherwise
    class(f) <- "classRepEnvironment"
    environment(f) <- ev
    setValidity(f, validity)
    assignClassDef(name, f, where)
    ## confirm the validity of the class definition (it may be incomplete)
    on.exit(removeClassDef(name, where))
    completeClassDefinition(name, f)
    ## if no error, allow assignment to stay
    on.exit()
    name
}

getClassDef <-
  ## Get the definition of the class supplied as a string.
  ## This is the unexpanded definition.  If you want all the information currently
  ## known, such as the complete list of superclasses, use `getClass(Class)'.
  function(Class, where = -1)
{
    cName <- classMetaName(Class)
    if(identical(where, 0))
        getFromClassMetaData(cName)
    else
        get(cName, where)
}

getClass <-
  ## Get the complete definition of the class supplied as a string,
  ## including all slots, etc. in classes that this class extends.
  function(Class, .Force = FALSE)
{
    value <- getClassDef(Class, 0)
    if(is.null(value)) {
        if(isClass(Class)) {
            value <- completeClassDefinition(Class)
            assignClassDef(Class, value, 0)
        }
        else if(!.Force && is.na(match(Class, .BasicClasses)))
        stop(paste("\"", Class, "\" is not a defined class"))
      ## else, return NULL.  This may change, if we force formal definitions
      ## for all classes, including (the tough ones) NULL, array, matrix, ts 
    }
    value
}

slot <-
  ## Get the value of the named slot.  This function does exact, not partial, matching of names,
  ## and the name must be one of the slot names specified in the class's definition.
  ##
  ## Because slots are stored as attributes, the validity check is not 100% guaranteed,
  ## but should be OK if nobody has "cheated" (e.g., by setting other attributes directly).
  function(object, name)
    .Call("R_get_slot", object, name, PACKAGE = "methods")

"slot<-" <-
  ## Set the value of the named slot.  Must be one of the slots in the class's definition.
  function(object, name, check = TRUE, value)
    .Call("R_set_slot", object, name, check, value, PACKAGE="methods")

checkSlotAssignment <- function(obj, name, value)
{
    ClassDef <- getClass(class(obj))
    slotDefs <- getProperties(ClassDef)
    slot <- elNamed(slotDefs, name)
    if(is.null(slot))
        stop(paste("\"", name, "\" is not a slot in class \"", class(object), "\"", sep = ""))
    if(identical(slot, class(value)))
       return(value)
    if(is(value, slot))
       return(as(value, slot))
    else
       stop(paste("Value supplied is not valid for slot \"", name, "\", is(value, \"", slot,
                   "\") is not TRUE", sep=""))
    NULL
}

        

## "@" <-
##   function(object, name)
##   .Internal(object@name)

"@<-" <-
   function(object, name, value) {
     arg <- substitute(name)
     if(is.name(arg))
       name <- as.character(arg)
     "slot<-"(object, name, TRUE, value)
   }

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
  function(Class, formal=TRUE)
{
    ## argument formal is for Splus compatibility & is ignored.  (All classes that
    ## are defined must have a class definition object.)
    exists(classMetaName(Class))
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
    ## the basic classes have fixed definitions
    if(!is.na(match(Class, .BasicClasses)))
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
        thisExtends <- names(getExtends(ClassDef))
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
                else {
                    ## is there a class to which we can coerce obj
                    ## that is then among the superclasses of Class?
                    extendsi <- names(getExtends(getClass(Classi)))
                    which <- match(extendsi, thisExtends)
                    which <- seq(along=which)[!is.na(which)]
                    if(length(which) == 1) {
                        Classi <- extendsi[[which]]
                        as(value, Classi) <- as(obj, Classi)
                    }
                    else
                        stop(paste("Can't use object of class \"", Classi,
                                   "\" in new():  Class \"", Class, "\" does not extend that class"))
                }
            }
        }
        if(length(elements)>0) {
            slotDefs <- getProperties(ClassDef)
            snames <- names(elements)
            if(any(duplicated(snames)))
                stop(paste("Duplicated slot names:",
                           paste(snames[duplicated(snames)], collapse = ", ")))
            which  <- match(snames, names(slotDefs))
            if(any(is.na(which)))
                stop(paste("Invalid names for properties of class ",
                           Class, ": ", paste(snames[is.na(which)], collapse=", ")))
            for(i in seq(along=snames)) {
                slotName <- el(snames, i)
                slotClass <- elNamed(slotDefs, slotName)
                slotVal <- el(elements, i)
                if(!.Force && !is(slotVal, slotClass))
                    stop(paste("Invalid object for slot \"", slotName,
                               "\", with class \"", class(slotVal), 
                               "\", should be or extend class \"", slotClass, "\"", sep = ""))
                slotVal <- as(slotVal, slotClass)
                slot(value, slotName, check = FALSE) <- slotVal
            }
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


validObject <- function(object, test = FALSE) {
  classDef <- getClass(class(object))
  anyStrings <- function(x) if(identical(x, TRUE)) character() else x
  ## perform, from bottom up, the default and any explicit validity tests
  ## First, validate the slots.
  errors <- character()
  slotTypes <- getProperties(classDef)
  slotNames <- names(slotTypes)
  for(i in seq(along=slotTypes)) {
    classi <- slotTypes[[i]]
    sloti <- slot(object, slotNames[[i]])
    if(!is(sloti, classi))
      errors <- c(errors, paste("is(object@",slotNames[[i]], ", \"",classi,
                                "\") failed", sep=""))
    else if(isClass(classi)) {
      errorsi <- Recall(sloti, TRUE)
      if(!identical(errorsi, TRUE))
        errors <- c(errors, paste("Slot ", slotNames[[i]], ": ",
                                  if(length(errorsi)>1) 1:length(errorsi) else "",
                                  errorsi, sep=""))
    }
  }
  extendType <- rev(getExtends(classDef))
  extends <- names(extendType); i <- 1
  while(length(errors) == 0 && i <= length(extends)) {
    superClass <- extends[[i]]
    test <- extendType[[i]]$test
    i <- i+1
    if(is.function(test) && !is(object, superClass))
      next ## skip conditional relations that don't hold for this object
    validityMethod <- getValidity(getClassDef(superClass))
    if(is(validityMethod, "function"))
      errors <- c(errors, anyStrings(validityMethod(as(object, superClass))))
    
  }
  validityMethod <- getValidity(classDef)
  if(length(errors) == 0 && is(validityMethod, "function")) {
    superClass <- class(object) ## for help in debugging
    errors <- c(errors, anyStrings(validityMethod(object)))
  }
  if(length(errors) > 0) {
    if(test)
      errors
    else if(length(errors) > 1)
      stop(paste("Invalid \"", class(object), "\" object: ", paste(paste(1:length(errors), errors, sep=": ")),
                 sep="", collapse = "\n"))
    else stop(paste("Invalid \"", class(object), "\" object: ", errors, sep=""))
  }
  else
    TRUE
}

setValidity <-
  function(ClassDef, method) {
    if(!isClassDef(ClassDef))
      ClassDef <- getClassDef(ClassDef)
    if(is.null(method) ||
      (is(method, "function") && length(formalArgs(method))==1))
      setInClassDef(ClassDef, ".Validity", method)
    else
      stop("validity method must be NULL or a function of one argument")
    resetClass(ClassDef)
  }

resetClass <-
    function(Class) {
        if(isClassDef(Class))
            Class <- getClassName(Class)
        cname <- classMetaName(Class)
        def <- getFromClassMetaData(cname)
        if(!is.null(def))
            removeFromClassMetaData(cname)
        Class
    }
