setClass <-
    ## Define Class to be an S-style class.
    function(Class, representation = list(), prototype = NULL,
             contains = character(), validity = NULL, access = list(),
             where = 1, version = .newExternalptr(), sealed = FALSE, package = getPackageName(where))
{
    if(isClass(Class) && getClassDef(Class)@sealed)
        stop(paste("\"", Class, "\" has a sealed class definition and cannot be redefined", sep=""))
    if(is(representation, "classRepresentation")) {
        ## supplied a class definition object
        classDef <- representation
        if(!(missing(prototype) && missing(contains) && missing(validity) && missing(access)
             && missing(version) && missing(package)))
            stop("Only arguments Class and where can be supplied when argument \"representation\" is a classRepresentation object")
        if(length(classDef@package)==0)
            classDef@package <- package # the default
        superClasses <- allNames(classDef@contains)
    }
    else { 
        ## catch the special case of a single class name as the representation
        if(is.character(representation) && length(representation) == 1 &&
           is.null(names(representation)))
            representation <- list(representation)
        slots <- nchar(allNames(representation))>0
        superClasses <- c(as.character(representation[!slots]), contains)
        properties <- representation[slots]
        classDef <- makeClassRepresentation(Class, properties,superClasses, prototype, package,
                                             validity, access, version, sealed)
        superClasses <- names(classDef@contains)
    }
    assignClassDef(Class, classDef, where)
    for(class2 in superClasses)
        setIs(Class, class2, where = where)
    ## confirm the validity of the class definition (it may be incomplete)
    msg <- trySilent(completeClassDefinition(Class, classDef))
    if(is(msg, "try-error")) {
        removeClass(Class, where=where)
        warning("Cannot complete class definition for \"", Class, "\" (",
                   msg, ") -- may conflict with previous definitions")
    }
    Class
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
        stop(paste("Duplicate class names among superclasses:", paste(includes[duplicated(includes)], collapse = ", ")), sep="")
    slots <- anames[nchar(anames)>0]
    if(any(duplicated(slots)))
       stop(paste("Duplicated slot names: ", paste(slots[duplicated(slots)], collapse=""), sep=""))
    value
}

### the version called prototype is the external interface.  But functions with argument
### named prototype in R cannot call the prototype function (until there is a methods namespace
### to allow methods::prototype(...)
prototype <- function(...)
    .prototype(...)

.prototype <- function(...) {
    props <- list(...)
    names <- allNames(props)
    data <- nchar(names) == 0
    if(any(data)) {
        if(sum(data) > 1)
            stop("only one data object (unnamed argument to prototype) allowed")
        obj <- props[[seq(along=data)[data] ]]
        props <- props[!data]
        names <- names[!data]
    }
    else
        obj <- list()
    for(i in seq(along = names))
        slot(obj, names[[i]], FALSE) <- props[[i]]
    new("classPrototypeDef", object = obj, slots = names)
}

makeClassRepresentation <-
  ## Set the Class Definition.
  ## The formal definition of the class is set according to the arguments.
  ##
  ## Users should call setClass instead of this function.
  function(name, slots = list(), superClasses = character(), prototype = NULL, package, validity = NULL, access = list(), version = .newExternalptr(), sealed = FALSE, virtual = NA)
{
    ## remove from the cached definition (only) right away
    removeClass(name, where = 0)
    if(!is.null(prototype) || length(slots)>0 || length(superClasses) >0) {
        ## collect information about slots, create prototype if needed
        pp <- reconcilePropertiesAndPrototype(name, slots, prototype, superClasses)
        slots <- pp$properties
        prototype <- pp$prototype
    }
    contains <- list();
    for(what in superClasses) {
        whatClassDef <- getClass(what)
        ## Create the SClassExtension objects (will be simple, possibly dataPart).
        ## The slots are supplied explicitly, since `name' is currently an undefined class
        elNamed(contains, what) <- makeExtends(name, what, slots = slots,
                                              classDef2 = whatClassDef)
    }
    validity <- .makeValidityMethod(name, validity)
    if(is.na(virtual)) {
        virtual <- testVirtual(slots, contains, prototype)
        if(virtual && !is.na(match("VIRTUAL", superClasses)))
            elNamed(contains, "VIRTUAL") <- NULL
    }
    newClassRepresentation(className = name, slots = slots,
                           contains = contains,
                           prototype = prototype,
                           virtual = virtual,
                           validity = validity,
                           access = access,
                           package = package,
                           versionKey = version,
                           sealed = sealed)
}

getClassDef <-
  ## Get the definition of the class supplied as a string.
  ## This is the unexpanded definition.  If you want all the information currently
  ## known, such as the complete list of superclasses, use `getClass(Class)'.
  function(Class, where = -1)
{
    cname <- classMetaName(Class)
    if(identical(where, 0))
        getFromClassMetaData(cname)
    ## much messing around below because of two problems with get/exists in R:
    ## 1. the inherits argument (TRUE by default, but FALSE & where = -1 don't mix
    ## 2. no way to say "get or return NULL"
    else if(identical(where, -1)) {
        if(exists(cname))
            get(cname)
        else
            NULL
    }        
    else if(exists(cname, where,inherits = FALSE))
        get(cname, where)
    else
        NULL
}

getClass <-
  ## Get the complete definition of the class supplied as a string,
  ## including all slots, etc. in classes that this class extends.
  function(Class, .Force = FALSE)
{
    value <- getClassDef(Class, 0)
    if(is.null(value)) {
        value  <- getClassDef(Class)
        if(is.null(value)) {
            if(!.Force)
                stop(paste("\"", Class, "\" is not a defined class", sep=""))
            else
                value <- makeClassRepresentation(Class, package = "base", virtual = TRUE)
        }
        else
            value <- completeClassDefinition(Class, value)
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
  function(object, name, check = TRUE, value) {
      if(check)
          value <- checkSlotAssignment(object, name, value)
      .Call("R_set_slot", object, name, value, PACKAGE="methods")
  }

checkSlotAssignment <- function(obj, name, value)
{
    ClassDef <- getClass(class(obj))
    slotDefs <- getProperties(ClassDef)
    slot <- elNamed(slotDefs, name)
    if(is.null(slot))
        stop(paste("\"", name, "\" is not a slot in class \"", class(obj), "\"", sep = ""))
    if(identical(slot, class(value)))
       return(value)
    if(is(value, slot))
       return(as(value, slot, strict=FALSE))
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
        Class <- .class1(x)
    names(getProperties(getClass(Class)))
}

removeClass <-
  ## Remove the definition of this class.
  function(Class, where = -1, removeSubclassLinks = TRUE)
{
    ## always remove the cached version, if any
    what <- classMetaName(Class)
    if(removeSubclassLinks && !identical(where, 0)) {
        fullDef <- trySilent(getClass(Class))
        if(is(fullDef, "try-error")) {
            warning("unable to get definition of \"", Class, "\" (",
                    fullDef, ")")
            fullDef <- NULL
        }
    }
    else
        fullDef <- getFromClassMetaData(what)
    if(!is.null(fullDef))
        removeFromClassMetaData(what)
    if(identical(where, 0))
    {}
    else {
        ClassDef <- getClassDef(Class)
        if(is.null(ClassDef)) {
            warning("\"", Class, "\" is not a class (no action taken)")
            return(FALSE)
        }
        if(identical(where, -1))
            rm(list=what, pos=find(what)[[1]])
        else
            rm(list=what, pos=where)
        ## remove extensions
        what <- extendsMetaName(ClassDef)
        extWhere <- find(what)
        for(pos in extWhere)
            rm(list = what, pos = pos)
        if(removeSubclassLinks && is(fullDef, "classRepresentation"))
            .removeSubclassLinks(ClassDef@className, classDef@package, fullDef@subclasses)
    }
    TRUE
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
  ## If other arguments are included, these are the values for named slots
  ## in the object, or an object that can be coerced into this class.
  ## Note that the basic vector classes, `"numeric"', etc. are implicitly defined,
  ## so one can use `new' for these classes.
  ###
  ### Unnamed arguments are objects from this class or a superclass.
  ##
  function(Class, ...)
{
    ## get the class definition, completing it if this is the first reference
    ## to this class in this session.
    ClassDef <- getClass(Class)
    if(identical(getVirtual(ClassDef), TRUE)) {
        stop("Trying to use new() on a virtual class")
    }
    else
        value <- getPrototype(ClassDef)
    class(value) <- Class
    initialize(value, ...)
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
    testFun <- extendType[[i]]$test
    i <- i+1
    if(is.function(testFun) && !is(object, superClass))
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
  function(Class, method, where = 1) {
    if(isClassDef(Class)) {
        ClassDef <- Class
        Class <- ClassDef@className
    }
    else {
      ClassDef <- getClassDef(Class)
  }
    method <- .makeValidityMethod(Class, method)
    if(is.null(method) ||
      (is(method, "function") && length(formalArgs(method))==1))
      ClassDef@validity <- method
    else
      stop("validity method must be NULL or a function of one argument")
    ## TO DO:  check the where argument against the package of the class def.
    assignClassDef(Class, ClassDef, where = where)
    resetClass(ClassDef)
  }

resetClass <-
    function(Class, 
             resetSubclasses = TRUE) {
        if(is(Class, "classRepresentation"))
            Class <- getClassName(Class)
        cname <- classMetaName(Class) ## TODO:  change to allow same name, different package
        def <- getFromClassMetaData(cname)
        if(!is.null(def)) {
            removeFromClassMetaData(cname)
            if(resetSubclasses)
                for(what in names(def@subclasses))
                    resetClass(what) # no further recursion needed if def is complete
        }
        Class
    }

## the (default) initialization:  becomes the default method when the function
## is made a generic by .InitMethodDefinitions

initialize <- function(.Object, ...) {
    args <- list(...)
    if(length(args) > 0) {
        Class <- class(.Object)
        ## the basic classes have fixed definitions
        if(!is.na(match(Class, .BasicClasses)))
            return(newBasic(Class, ...))
        ClassDef <- getClass(Class)
        ## separate the slots, superclass objects
        snames <- allNames(args)
        which <- nchar(snames)>0
        elements <- args[which]
        supers <- args[!which]
        thisExtends <- names(getExtends(ClassDef))
        slotDefs <- getProperties(ClassDef)
        dataPart <- elNamed(slotDefs, ".Data")
        if(is.null(dataPart)) dataPart <- "missing"
        if(length(supers) > 0) {
            for(i in rev(seq(along = supers))) {
                obj <- el(supers, i)
                Classi <- .class1(obj)
                if(identical(Classi, Class))
                    .Object <- obj
                else if(extends(Classi, Class))
                    .Object <- as(obj, Class, strict=FALSE)
                else if(extends(Classi, dataPart))
                    .Object@.Data <- obj
                else if(extends(Class, Classi))
                    as(.Object, Classi) <- obj
                else {
                    ## is there a class to which we can coerce obj
                    ## that is then among the superclasses of Class?
                    extendsi <- extends(Classi)[-1]
                    ## look for the common extensions, choose the first
                    ## one in the extensions of Class
                    which <- match(thisExtends, extendsi)
                    which <- seq(along=which)[!is.na(which)]
                    if(length(which) >0 ) {
                        Classi <- thisExtends[which[1]]
                        as(.Object, Classi) <- as(obj, Classi, strict = FALSE)
                    }
                    else
                        stop(paste("Can't use object of class \"", Classi,
                                   "\" in new():  Class \"", Class, "\" does not extend that class", sep=""))
                }
            }
        }
        if(length(elements)>0) {
            snames <- names(elements)
            if(any(duplicated(snames)))
                stop(paste("Duplicated slot names:",
                           paste(snames[duplicated(snames)], collapse = ", ")), sep="")
            which  <- match(snames, names(slotDefs))
            if(any(is.na(which)))
                stop(paste("Invalid names for slots of class ",
                           Class, ": ", paste(snames[is.na(which)], collapse=", "), sep=""))
            for(i in seq(along=snames)) {
                slotName <- el(snames, i)
                slotClass <- elNamed(slotDefs, slotName)
                slotVal <- el(elements, i)
                if(!is(slotVal, slotClass))
                    stop(paste("Invalid object for slot \"", slotName,
                               "\", with class \"", class(slotVal), 
                               "\", should be or extend class \"", slotClass, "\"", sep = ""))
                slotVal <- as(slotVal, slotClass, strict = FALSE)
                slot(.Object, slotName, check = FALSE) <- slotVal
            }
        }
    }
    .Object
}

findClass <- function(Class) {
    if(!is.character(Class))
        Class <- getClassName(Class)
    what <- classMetaName(Class)
    where <- search()
    ok <- logical(length(where))
    for(i in seq(along=where))
        ok[i] <- exists(what, where[i], inherits = FALSE)
    where[ok]
}
