setClass <-
    ## Define Class to be an S-style class.
    function(Class, representation = list(), prototype = NULL,
             contains = character(), validity = NULL, access = list(),
             where = topenv(parent.frame()), version = .newExternalptr(), sealed = FALSE, package = getPackageName(where))
{
    oldDef <- getClassDef(Class, where)
    if(is(oldDef, "classRepresentation") && oldDef@sealed)
        stop(gettextf("\"%s\" has a sealed class definition and cannot be redefined", Class), domain = NA)
    if(is(representation, "classRepresentation")) {
        ## supplied a class definition object
        classDef <- representation
        if(!(missing(prototype) && missing(contains) && missing(validity) && missing(access)
             && missing(version) && missing(package)))
            stop("only arguments 'Class' and 'where' can be supplied when argument 'representation' is a 'classRepresentation' object")
        if(length(classDef@package)==0)
            classDef@package <- package # the default
        superClasses <- allNames(classDef@contains)
    }
    else {
        ## catch the special case of a single class name as the representation
        if(is.character(representation) && length(representation) == 1 &&
           is.null(names(representation)))
            representation <- list(representation)
        slots <- nchar(allNames(representation)) > 0
        superClasses <- c(as.character(representation[!slots]), contains)
        properties <- representation[slots]
        classDef <- makeClassRepresentation(Class, properties,superClasses, prototype, package,
                                             validity, access, version, sealed, where = where)
        superClasses <- names(classDef@contains)
    }
    classDef <- completeClassDefinition(Class, classDef, where, doExtends = FALSE)
    oldDef <- getClassDef(Class, where)
    if(length(superClasses) == 0)
        assignClassDef(Class, classDef, where)
    else {
        sealed <- classDef@sealed
        classDef@sealed <- FALSE # to allow setIs to work anyway; will be reset later
        assignClassDef(Class, classDef, where)
        badContains <- character()
        for(class2 in superClasses)
            if(is(try(setIs(Class, class2, classDef = classDef, where = where)), "try-error"))
                badContains <- c(badContains, class2)
        if(length(badContains) > 0) {
            msg <- paste(dQuote(badContains), collapse = ", ")
            if(is(try(removeClass(Class, where)), "try-error"))
                stop(gettextf("error in contained classes (%s) for class \"%s\" and unable to remove definition from '%s'",
                              msg, Class, getPackageName(where)), domain = NA)
            if(is.null(oldDef))
                stop(gettextf("error in contained classes (%s) for class \"%s\"; class definition removed from '%s'",
                              msg, Class, getPackageName(where)), domain = NA)
            else if(is(try(setClass(Class, oldDef, where=where)), "try-error"))
                stop(gettextf("error in contained classes (%s) for class \"%s\" and unable to restore previous definition from '%s'",
                              msg, Class, getPackageName(where)), domain = NA)
            else
                stop(gettextf("error in contained classes (%s) for class \"%s\"; previous definition restored to '%s'",
                              msg, Class, getPackageName(where)), domain = NA)
        }
        if(sealed) {
            classDef@sealed <- TRUE
            assignClassDef(Class, classDef, where)
        }
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
            stop(gettextf("element %d of the representation was not a single character string", i), domain = NA)
    }
    includes <- as.character(value[nchar(anames)==0])
    if(any(duplicated(includes)))
        stop(gettextf("duplicate class names among superclasses: %s",
                      paste(dQuote(includes[duplicated(includes)]),
                            collapse = ", ")),
             domain = NA)
    slots <- anames[nchar(anames)>0]
    if(any(duplicated(slots)))
       stop(gettextf("duplicated slot names: %s",
                     paste(sQuote(slots[duplicated(slots)]), collapse="")),
            domain = NA)
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
    dataPart <- any(data)
    if(dataPart) {
        if(sum(data) > 1)
            stop("only one data object (unnamed argument to prototype) allowed")
        obj <- unclass(props[[seq(along=data)[data] ]])
        props <- props[!data]
        names <- names[!data]
    }
    else
        obj <- list()
    for(i in seq(along = names))
        slot(obj, names[[i]], FALSE) <- props[[i]]
    new("classPrototypeDef", object = obj, slots = names, dataPart = dataPart)
}

makeClassRepresentation <-
  ## Set the Class Definition.
  ## The formal definition of the class is set according to the arguments.
  ##
  ## Users should call setClass instead of this function.
  function(name, slots = list(), superClasses = character(), prototype = NULL, package, validity = NULL, access = list(), version = .newExternalptr(), sealed = FALSE, virtual = NA, where)
{
    if(!is.null(prototype) || length(slots)>0 || length(superClasses) >0) {
        ## collect information about slots, create prototype if needed
        pp <- reconcilePropertiesAndPrototype(name, slots, prototype, superClasses, where)
        slots <- pp$properties
        prototype <- pp$prototype
    }
    contains <- list();
    if(nchar(package)>0)
        packageSlot(name) <- package
    for(what in superClasses) {
        if(is(what, "classRepresentation"))
            whatClassDef <- what
        else if(is.null(packageSlot(what)))
            whatClassDef <- getClass(what, where = where)
        else
            whatClassDef <- getClass(what)
        what <- whatClassDef@className # includes package name as attribute
        ## Create the SClassExtension objects (will be simple, possibly dataPart).
        ## The slots are supplied explicitly, since `name' is currently an undefined class
        elNamed(contains, what) <- makeExtends(name, what, slots = slots,
                                              classDef2 = whatClassDef, package = package)
    }
    validity <- .makeValidityMethod(name, validity)
    if(is.na(virtual)) {
        virtual <- testVirtual(slots, contains, prototype, where)
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
  function(Class, where = topenv(parent.frame()), package = packageSlot(Class))
{
    ## FIXME:  really wants to be is(Class, "classRepresentation") but
    ## generates inf. loop in booting methods package (also for new())
    if(.identC(class(Class), "classRepresentation"))
        return(Class)
    if(length(Class)>1)
        ## S3 class; almost certainly has no packageSLot, but we'll continue anyway
        cname <- classMetaName(Class[[1]])
    else
        cname <- classMetaName(Class)
    value <- NULL
    ## a string with a package slot strongly implies the class definition
    ## should be in that package.
    if(!is.null(package)) {
        whereP <- .requirePackage(package)
        if(exists(cname, whereP))
            value <- get(cname, whereP)
    }
    if(is.null(value) && exists(cname, where))
        value <- get(cname, where)
    value
}

getClass <-
  ## Get the complete definition of the class supplied as a string,
  ## including all slots, etc. in classes that this class extends.
  function(Class, .Force = FALSE,
	   where = .classEnv(Class, topenv(parent.frame()), FALSE))
{
    value <- getClassDef(Class, where)
    if(is.null(value)) {
	if(!.Force)
	    stop(gettextf("\"%s\" is not a defined class", Class), domain = NA)
	else
	    value <- makeClassRepresentation(Class, package = "base",
					     virtual = TRUE, where = where)
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
    cl <- class(obj)
    ClassDef <- getClass(cl) # fails if cl not a defined class (!)
    slotClass <- elNamed(ClassDef@slots, name)
    if(is.null(slotClass))
        stop(gettextf("\"%s\" is not a slot in class \"%s\"", name, class(obj)),
             domain = NA)
    valueClass <- class(value)
    if(.identC(slotClass, valueClass))
       return(value)
    ## check the value, but be careful to use the definition of the slot's class from
    ## the class environment of obj (change validObject too if a better way is found)
    ok <- possibleExtends(valueClass, slotClass, ClassDef2 = getClassDef(slotClass, where = .classEnv(ClassDef)))
    if(identical(ok, FALSE))
       stop(gettextf("assignment of an object of class \"%s\" is not valid for slot '%s' in an object of class \"%s\"; is(value, \"%s\") is not TRUE",
                     class(value),  name, class(obj), slotClass),
            domain = NA)
    else if(identical(ok, TRUE))
        value
    else
       as(value, slotClass, strict=FALSE, ext = ok)
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

##  The names of the class's slots.  The argument is either the name
##  of a class, or an object from the relevant class.
slotNames <- function(x)
    if(is(x, "classRepresentation")) names(x@slots) else .slotNames(x)

.slotNames <- function(x)
{
    classDef <-
	getClassDef(if(is.character(x) && length(x) == 1) x else class(x))
    if(is.null(classDef))
	character()
    else
	names(classDef@slots)
}


removeClass <-  function(Class, where) {
    if(missing(where)) {
        where <- findClass(Class, topenv(parent.frame()))
        if(length(where) == 0) {
            warning(gettextf("\"%s\" is not a class (no action taken)", Class),
                    domain = NA)
            return(FALSE)
        }
        if(length(where) > 1)
            warning(gettextf("class \"%s\" has multiple definitions visible; only the first removed", Class), domain = NA)
        where <- where[[1]]
    }
    what <- classMetaName(Class)
    rm(list=what, pos=where)
    TRUE
}


isClass <-
  ## Is this a formally defined class?
  function(Class, formal=TRUE, where = topenv(parent.frame()))
{
    ## argument formal is for Splus compatibility & is ignored.  (All classes that
    ## are defined must have a class definition object.)
    if(missing(where))
        !is.null(getClassDef(Class))
    else
        !is.null(getClassDef(Class, where))
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
    ## FIXME:  really wants to be is(Class, "classRepresentation") but
    ## generates inf. loop in booting methods package (also for getClassDef)
    if(.identC(class(Class), "classRepresentation"))
        ClassDef <- Class
    else
        ClassDef <- getClass(Class, where = topenv(parent.frame()))
    if(identical(ClassDef@virtual, TRUE)) {
        stop("trying to use new() on a virtual class")
    }
    else
        value <- ClassDef@prototype
    class(value) <- ClassDef@className
    initialize(value, ...)
}

getClasses <-
  ## The names of all the classes formally defined on `where'.
  ## If called with no argument, all the classes currently known in the session
  ## (which does not include classes that may be defined on one of the attached
  ## libraries, but have not yet been used in the session).
  function(where = .externalCallerEnv(), inherits = missing(where))
{
    pat <- paste("^",classMetaName(""), sep="")
    if(inherits) {
        evList <- .parentEnvList(where)
        clNames <- character()
        for(ev in evList)
            clNames <- c(clNames, objects(ev, pattern = pat, all.names = TRUE))
        clNames <- unique(clNames)
    }
    else
        clNames <- objects(where, pattern = pat, all.names = TRUE)
    ## strip off the leading pattern (this implicitly assumes the characters
    ## in classMetaName("") are either "." or not metacharacters
    substring(clNames, nchar(pat))
}


validObject <- function(object, test = FALSE)
{
    Class <- class(object)
    classDef <- getClassDef(Class)
    where <- .classEnv(classDef)
    anyStrings <- function(x) if(identical(x, TRUE)) character() else x
    ## perform, from bottom up, the default and any explicit validity tests
    ## First, validate the slots.
    errors <- character()
    slotTypes <- classDef@slots
    slotNames <- names(slotTypes)
    for(i in seq(along=slotTypes)) {
        classi <- slotTypes[[i]]
        sloti <- slot(object, slotNames[[i]])
        ## note that the use of possibleExtends is shared with checkSlotAssignment(), in case a
        ## future revision improves on it!
        ok <- possibleExtends(class(sloti), classi, ClassDef2 = getClassDef(classi, where = where))
        if(identical(ok, FALSE))
            errors <- c(errors,
                        paste("invalid object for slot \"", slotNames[[i]],
                              "\" in class \"", Class,
                              "\": got class \"", class(sloti),
                              "\", should be or extend class \"", classi, "\"", sep = ""))
    }
    extends <- rev(classDef@contains); i <- 1
    while(length(errors) == 0 && i <= length(extends)) {
        exti <- extends[[i]]
        superClass <- exti@superClass
        i <- i+1
        if(!exti@simple && !is(object, superClass))
            next ## skip conditional relations that don't hold for this object
        superDef <- getClassDef(superClass, where = where)
        if(is.null(superDef)) {
            errors <- c(errors, paste("superclass \"", superClass, "\" not defined in the environment of the object's class", sep=""))
            next
        }
        validityMethod <- superDef@validity
        if(is(validityMethod, "function"))
            errors <- c(errors, anyStrings(validityMethod(as(object, superClass))))
    }
    validityMethod <- classDef@validity
    if(length(errors) == 0 && is(validityMethod, "function")) {
        errors <- c(errors, anyStrings(validityMethod(object)))
    }
    if(length(errors) > 0) {
        if(test)
            errors
        else {
            msg <- gettextf("invalid class \"%s\" object:", Class)
            if(length(errors) > 1)
                stop(paste(msg,
                           paste(paste(1:length(errors), errors, sep=": ")),
                           collapse = "\n"), domain = NA)
            else stop(msg, " ", errors, domain = NA)
    }
}
  else
    TRUE
}

setValidity <-
  function(Class, method, where = topenv(parent.frame())) {
    if(isClassDef(Class)) {
        ClassDef <- Class
        Class <- ClassDef@className
    }
    else {
      ClassDef <- getClassDef(Class, where)
  }
    method <- .makeValidityMethod(Class, method)
    if(is.null(method) ||
      (is(method, "function") && length(formalArgs(method))==1))
      ClassDef@validity <- method
    else
      stop("validity method must be NULL or a function of one argument")
    ## TO DO:  check the where argument against the package of the class def.
    assignClassDef(Class, ClassDef, where = where)
    resetClass(Class, ClassDef, where = where)
  }

resetClass <- function(Class, classDef, where) {
        if(is(Class, "classRepresentation")) {
            classDef <- Class
            Class <- Class@className
            if(missing(where))
                where <- .classDefEnv(classDef)
        }
        else {
            if(missing(where)) {
                if(missing(classDef))
                    where <- findClass(Class, unique = "resetting the definition")[[1]]
                else
                    where <- .classDefEnv(classDef)
            }
            if(missing(classDef)) {
                classDef <- getClassDef(Class, where)
                if(is.null(classDef)) {
                    warning(gettextf("class \"%s\" not found on '%s'; 'resetClass' will have no effect", Class, getPackageName(where)), domain = NA)
                    return(classDef)
                }
            }
            else if(!is(classDef, "classRepresentation"))
                stop(gettextf("argument 'classDef' must be a string or a class representation; got an object of class \"%s\"", class(classDef)), domain = NA)
            package <- getPackageName(where)
        }
        if(classDef@sealed)
            warning(gettextf("class \"%s\" is sealed; 'resetClass' will have no effect", Class), domain = NA)
        else {
            classDef <-  .uncompleteClassDefinition(classDef)
            classDef <- completeClassDefinition(Class, classDef, where)
            assignClassDef(Class, classDef, where)
        }
        classDef
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
        thisExtends <- names(ClassDef@contains)
        slotDefs <- ClassDef@slots
        dataPart <- elNamed(slotDefs, ".Data")
        if(is.null(dataPart)) dataPart <- "missing"
        if(length(supers) > 0) {
            for(i in rev(seq(along = supers))) {
                obj <- el(supers, i)
                Classi <- class(obj)
                ## test some cases that let information be copied into the
                ## object, ordered from more to less:  all the slots in the
                ## first two cases, some in the 3rd, just the data part in 4th
                if(.identC(Classi[[1]], Class))
                    .Object <- obj
                else if(extends(Classi, Class))
                    .Object <- as(obj, Class, strict=FALSE)
                else if(extends(Class, Classi))
                    as(.Object, Classi) <- obj
                else if(extends(Classi, dataPart))
                    .Object@.Data <- obj
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
                        stop(gettextf("cannot use object of class \"%s\" in new():  class \"%s\" does not extend that class", Classi, Class), domain = NA)
                }
            }
        }
        if(length(elements)>0) {
            snames <- names(elements)
            if(any(duplicated(snames)))
                stop(gettextf("duplicated slot names: %s",
                              paste(sQuote(snames[duplicated(snames)]),
                                    collapse = ", ")), domain = NA)
            which  <- match(snames, names(slotDefs))
            if(any(is.na(which)))
                stop(gettextf("invalid names for slots of class \"%s\": %s",
                              Class,
                              paste(snames[is.na(which)], collapse=", ")),
                     domain = NA)
            for(i in seq(along=snames)) {
                slotName <- el(snames, i)
                slotClass <- elNamed(slotDefs, slotName)
                slotClassDef <- getClassDef(slotClass, package=ClassDef@package)
                slotVal <- el(elements, i)
                ## perform non-strict coercion, but leave the error messages for
                ## values not conforming to the slot definitions to validObject(),
                ## hence the check = FALSE argument in the slot assignment
                if(!.identC(class(slotVal), slotClass)
                   && !is.null(slotClassDef) ) {
                    valClass <- class(slotVal)
                    valClassDef <- getClassDef(valClass, package = ClassDef@package)
                    if(!identical(possibleExtends(valClass, slotClass,
                                         valClassDef, slotClassDef), FALSE))
                        slotVal <- as(slotVal, slotClass, strict = FALSE)
                }
                slot(.Object, slotName, check = FALSE) <- slotVal
            }
        }
        validObject(.Object)
     }
    .Object
}

findClass <- function(Class, where = topenv(parent.frame()), unique = "") {
    if(is(Class, "classRepresentation")) {
        pkg <- Class@package
        Class <- Class@className
    }
    else
        pkg <- ""
    if(missing(where) && nchar(pkg))
            where <- .requirePackage(pkg)
    else
        where <- as.environment(where)
    what <- classMetaName(Class)
    where <- .findAll(what, where)
    if(length(where) != 1 && nchar(unique)>0) {
            if(length(where) == 0)
                stop(gettextf("no definition of \"%s\" to use for %s",
                              Class, unique), domain = NA)
            if(length(where) > 1) {
                where <- where[[1]]
                ## problem: 'unique'x is text passed in, so do not translate
                warning(sprintf("multiple definitions of class \"%s\" visible; using the definition on package '%s' for %s",
                                 Class, getPackageName(where), unique),
                        domain = NA)
            }
    }
    where
}

isSealedClass <- function(Class, where = topenv(parent.frame())) {
    if(is.character(Class))
            Class <- getClass(Class, TRUE, where)
    if(!is(Class, "classRepresentation"))
        FALSE
    else
        Class@sealed
}

sealClass <- function(Class, where = topenv(parent.frame())) {
    if(missing(where))
        where <- findClass(Class, unique = "sealing the class", where = where)
    classDef <- getClassDef(Class, where)
    if(!classDef@sealed) {
        classDef@sealed <- TRUE
        assignClassDef(Class, classDef, where)
    }
    invisible(classDef)
}

