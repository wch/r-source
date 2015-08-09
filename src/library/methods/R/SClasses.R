#  File src/library/methods/R/SClasses.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

setClass <-
    ## Define Class to be an S4 class.
    function(Class, representation = list(), prototype = NULL,
             contains = character(), validity = NULL, access = list(),
             where = topenv(parent.frame()), version = .newExternalptr(),
             sealed = FALSE, package = getPackageName(where),
             S3methods = FALSE, slots)
{
    oldDef <- getClassDef(Class, where)
    if(is(oldDef, "classRepresentation") && oldDef@sealed)
        stop(gettextf("%s has a sealed class definition and cannot be redefined",
                      dQuote(Class)),
             domain = NA)
    if(!missing(slots)) {
        ## The modern version consistent with reference classes
        ## Arguments slots= and contains= are used, representation must not be
        if(!missing(representation))
            stop("Argument \"representation\" cannot be used if argument \"slots\" is supplied")
        properties <- inferProperties(slots, "slot")
        classDef <- makeClassRepresentation(Class, properties,contains, prototype, package,
                                             validity, access, version, sealed, where = where)
        superClasses <- names(classDef@contains)
    }
    else if(is(representation, "classRepresentation")) {
        ## supplied a class definition object
        classDef <- representation
        if(!(missing(prototype) && missing(contains) && missing(validity) && missing(access)
             && missing(version) && missing(package)))
            stop("only arguments 'Class' and 'where' can be supplied when argument 'representation' is a 'classRepresentation' object")
        if(length(classDef@package) == 0L)
            classDef@package <- package # the default
        superClasses <- allNames(classDef@contains)
    }
    else {
        ## catch the special case of a single class name as the representation
        if(is.character(representation) && length(representation) == 1L &&
           is.null(names(representation)))
            representation <- list(representation)
        slots <- nzchar(allNames(representation))
        superClasses <- c(as.character(representation[!slots]), contains)
        properties <- representation[slots]
        classDef <- makeClassRepresentation(Class, properties,superClasses, prototype, package,
                                             validity, access, version, sealed, where = where)
        superClasses <- names(classDef@contains)
    }
    classDef <- completeClassDefinition(Class, classDef, where, doExtends = FALSE)
    ## uncache an old definition for this package, if one is cached
    .uncacheClass(Class, classDef)
    if(length(superClasses) > 0L) {
        sealed <- classDef@sealed
        classDef@sealed <- FALSE # to allow setIs to work anyway; will be reset later
        assignClassDef(Class, classDef, where)
        badContains <- character()
        for(class2 in superClasses) {
            if(is(try(setIs(Class, class2, classDef = classDef, where = where)), "try-error"))
                badContains <- c(badContains, class2)
            else { # update class definition
                classDef <- getClassDef(Class, where = where)
                if(is.null(classDef))
                  stop(sprintf("internal error: definition of class %s not properly assigned",
                                dQuote(Class)),
                       domain = NA)
            }
          }
        if(length(badContains)) {
            msg <- paste(.dQ(badContains), collapse = ", ")
            if(is(try(removeClass(Class, where)), "try-error"))
                stop(gettextf("error in contained classes (%s) for class %s and unable to remove definition from %s",
                              msg, dQuote(Class),
                              sQuote(getPackageName(where))),
                     domain = NA)
            if(is.null(oldDef))
                stop(gettextf("error in contained classes (%s) for class %s; class definition removed from %s",
                              msg, dQuote(Class),
                              sQuote(getPackageName(where))),
                     domain = NA)
            else if(is(try(setClass(Class, oldDef, where=where)), "try-error"))
                stop(gettextf("error in contained classes (%s) for class %s and unable to restore previous definition from %s",
                              msg, dQuote(Class),
                              sQuote(getPackageName(where))),
                     domain = NA)
            else
                stop(gettextf("error in contained classes (%s) for class %s; previous definition restored to %s",
                              msg, dQuote(Class),
                              sQuote(getPackageName(where))),
                     domain = NA)
        }
        if(length(attr(classDef@contains, "conflicts")) > 0)
          .reportSuperclassConflicts(Class, classDef@contains, where)
        .checkRequiredGenerics(Class, classDef, where)
        if(sealed) {
            classDef@sealed <- TRUE
        }
    }
    if(S3methods)
      classDef <- .setS3MethodsOn(classDef)
    assignClassDef(Class, classDef, where)
    invisible(classGeneratorFunction(classDef, where))
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
    for(i in seq_along(value)) {
        ei <- el(value, i)
        if(!is.character(ei) || length(ei) != 1L)
            stop(gettextf("element %d of the representation was not a single character string", i), domain = NA)
    }
    includes <- as.character(value[!nzchar(anames)])
    if(anyDuplicated(includes))
        stop(gettextf("duplicate class names among superclasses: %s",
                      paste(.dQ(includes[duplicated(includes)]),
                            collapse = ", ")),
             domain = NA)
    slots <- anames[nzchar(anames)]
    if(anyDuplicated(slots)) {
        dslots <- slots[duplicated(slots)]
        stop(sprintf(ngettext(length(dslots),
                              "duplicated slot name: %s",
                              "duplicated slot names: %s"),
                     paste(sQuote(dslots), collapse="")),
             domain = NA)
    }
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
    data <- !nzchar(names)
    dataPart <- any(data)
    if(dataPart) {
        if(sum(data) > 1)
            stop("only one data object (unnamed argument to prototype) allowed")
        obj <- unclass(props[[seq_along(data)[data] ]])
        props <- props[!data]
        names <- names[!data]
    }
    else
        obj <- defaultPrototype()
    for(i in seq_along(names))
        slot(obj, names[[i]], FALSE) <- props[[i]]
    new("classPrototypeDef", object = obj, slots = names, dataPart = dataPart)
}

makeClassRepresentation <-
  ## Set the Class Definition.
  ## The formal definition of the class is set according to the arguments.
  ##
  ## Users should call setClass instead of this function.
  function(name, slots = list(), superClasses = character(), prototype = NULL,
	   package, validity = NULL, access = list(), version = .newExternalptr(),
	   sealed = FALSE, virtual = NA, where)
{
    if(any(superClasses %in% .AbnormalTypes))
        superClasses <- .addAbnormalDataType(superClasses)
    if(!is.null(prototype) || length(slots) || length(superClasses)) {
        ## collect information about slots, create prototype if needed
        pp <- reconcilePropertiesAndPrototype(name, slots, prototype, superClasses, where)
        slots <- pp$properties
        prototype <- pp$prototype
    }
    contains <- list()
    if(nzchar(package))
        packageSlot(name) <- package
    for(what in superClasses) {
	whatClassDef <-
	    if(is(what, "classRepresentation"))
		what
	    else if(is.null(packageSlot(what)))
		getClass(what, where = where)
	    else
		getClass(what)
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
    # new() must return an S4 object, except perhaps for basic classes
    if(!is.null(prototype) && is.na(match(name, .BasicClasses)))
      prototype <- .asS4(prototype)
    if(".S3Class" %in% names(slots))
      prototype <- .addS3Class(name, prototype, contains, where)
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
  function(Class, where = topenv(parent.frame()), package = packageSlot(Class),
           inherits = TRUE, resolve.msg = getOption("getClass.msg", default=TRUE))
{
    value <- if(inherits) #includes both the lookup and Class being already a definition
	.getClassFromCache(Class, where, package=package, resolve.msg=resolve.msg)
    ## else NULL # want to force a search for the metadata in this case (Why?)
    if(is.null(value)) {
	cname <-
	    classMetaName(if(length(Class) > 1L)
			  ## S3 class; almost certainly has no packageSlot,
			  ## but we'll continue anyway
			  Class[[1L]] else Class)
	## a string with a package slot strongly implies the class definition
	## should be in that package.
	if(identical(nzchar(package), TRUE)) {
	    whereP <- .requirePackage(package)
	    value <- get0(cname, whereP, inherits = inherits) # NULL if not existing
	}
	if(is.null(value))
	    value <- get0(cname, where, inherits = inherits) # NULL if not existing
    }
    value
}

getClass <-
  ## Get the complete definition of the class supplied as a string,
  ## including all slots, etc. in classes that this class extends.
  function(Class, .Force = FALSE,
	   where = .classEnv(Class, topenv(parent.frame()), FALSE),
           resolve.msg = getOption("getClass.msg", default=TRUE))
{
    value <- .getClassFromCache(Class, where, resolve.msg=resolve.msg) # the quick way
    if(is.null(value)) {
        value <- getClassDef(Class, where, resolve.msg=resolve.msg) # searches
        if(is.null(value)) {
            if(!.Force)
                stop(gettextf("%s is not a defined class",
                              dQuote(Class)),
                     domain = NA)
            else
                value <- makeClassRepresentation(Class, package = "base",
                                                 virtual = TRUE, where = where)
        }
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
    .Call(C_R_get_slot, object, name)

"slot<-" <-
  ## Set the value of the named slot.  Must be one of the slots in the class's definition.
  function(object, name, check = TRUE, value) {
      if(check)
          value <- checkSlotAssignment(object, name, value)
      .Call(C_R_set_slot, object, name, value)
      ## currently --> R_do_slot_assign() in ../../../main/attrib.c
  }

## ". - hidden" since one should typically rather use is(), extends() etc:
.hasSlot <- function(object, name)
    .Call(C_R_hasSlot, object, name)

checkSlotAssignment <- function(obj, name, value)
{
    cl <- class(obj)
    ClassDef <- getClass(cl) # fails if cl not a defined class (!)
    slotClass <- elNamed(ClassDef@slots, name)
    if(is.null(slotClass))
        stop(gettextf("%s is not a slot in class %s",
                      sQuote(name), dQuote(cl)),
             domain = NA)
    valueClass <- class(value)
    if(.identC(slotClass, valueClass))
       return(value)
    ## check the value, but be careful to use the definition of the slot's class from
    ## the class environment of obj (change validObject too if a better way is found)
    ok <- possibleExtends(valueClass, slotClass,
                          ClassDef2 = getClassDef(slotClass, where = .classEnv(ClassDef)))
    if(identical(ok, FALSE))
       stop(gettextf("assignment of an object of class %s is not valid for slot %s in an object of class %s; is(value, \"%s\") is not TRUE",
		     dQuote(valueClass), sQuote(name), dQuote(cl), slotClass),
            domain = NA)
    else if(identical(ok, TRUE))
        value
    else
       as(value, slotClass, strict=FALSE, ext = ok)
}

## slightly simpler verison to be called from do_attrgets()
checkAtAssignment <- function(cl, name, valueClass)
{
    ClassDef <- getClass(cl) # fails if cl not a defined class (!)
    slotClass <- elNamed(ClassDef@slots, name)
    if(is.null(slotClass))
        stop(gettextf("%s is not a slot in class %s",
                      sQuote(name), dQuote(cl)),
             domain = NA)
    if(.identC(slotClass, valueClass))
       return(TRUE)
    ## check the value, but be careful to use the definition of the slot's class from
    ## the class environment of obj (change validObject too if a better way is found)
    ok <- possibleExtends(valueClass, slotClass,
                          ClassDef2 = getClassDef(slotClass, where = .classEnv(ClassDef)))
    if(identical(ok, FALSE))
       stop(gettextf("assignment of an object of class %s is not valid for @%s in an object of class %s; is(value, \"%s\") is not TRUE",
		     dQuote(valueClass), sQuote(name), dQuote(cl), slotClass),
            domain = NA)
    TRUE
}

## Now a primitive in base
## "@<-" <-
##    function(object, name, value) {
##      arg <- substitute(name)
##      if(is.name(arg))
##        name <- as.character(arg)
##      "slot<-"(object, name, TRUE, value)
##    }

##  The names of the class's slots.  The argument is either the name
##  of a class, or an object from the relevant class.

## NOTA BENE:  .slotNames() shouldn't be needed,
##             rather slotNames() should be changed (to work like .slotNames())!
slotNames <- function(x)
    if(is(x, "classRepresentation")) names(x@slots) else .slotNames(x)

.slotNames <- function(x)
{
    classDef <- getClassDef(
	if(!isS4(x) && is.character(x) && length(x) == 1L) x else class(x))
    if(is.null(classDef))
	character()
    else
	names(classDef@slots)
}


removeClass <-  function(Class, where = topenv(parent.frame()),
                         resolve.msg = getOption("removeClass.msg", default=TRUE)) {
    if(missing(where)) {
       classEnv <- .classEnv(Class, where, FALSE)
        classWhere <- findClass(Class, where = classEnv)
        if(length(classWhere) == 0L) {
            warning(gettextf("class definition for %s not found (no action taken)",
                             dQuote(Class)),
                    domain = NA)
            return(FALSE)
        }
        if(length(classWhere) > 1L)
	    warning(gettextf(
		"class %s has multiple definitions visible; only the first removed",
                             dQuote(Class)),
                    domain = NA)
        classWhere <- classWhere[[1L]]
    }
    else classWhere <- where
    classDef <- getClassDef(Class, where=classWhere)
    if(length(classDef@subclasses)) {
      subclasses <- names(classDef@subclasses)
      found <- vapply(subclasses, isClass, NA, where = where, USE.NAMES=TRUE)
      for(what in subclasses[found])
          .removeSuperClass(what, Class, resolve.msg=resolve.msg)
    }
    .removeSuperclassBackRefs(Class, classDef, classWhere)
    .uncacheClass(Class, classDef)
    .undefineMethod("initialize", Class, classWhere)
    what <- classMetaName(Class)
    rm(list=what, pos=classWhere)
    TRUE
}


isClass <-
  ## Is this a formally defined class?
  function(Class, formal=TRUE, where = topenv(parent.frame()))
    ## argument formal is for Splus compatibility & is ignored.  (All classes that
    ## are defined must have a class definition object.)
    !is.null(getClassDef(Class, where))

### TODO   s/Class/._class/  -- in order to allow 'Class' as regular slot name
new <-
  ## Generate an object from the specified class.
  ##
  ## Note that the basic vector classes, `"numeric"', etc. are implicitly defined,
  ## so one can use `new' for these classes.
  ##
  function(Class, ...)
{
    ClassDef <- getClass(Class, where = topenv(parent.frame()))
    value <- .Call(C_new_object, ClassDef)
    initialize(value, ...)
}

getClasses <-
  ## The names of all the classes formally defined on `where'.
  ## If called with no argument, all the classes currently known in the session
  ## (which does not include classes that may be defined on one of the attached
  ## libraries, but have not yet been used in the session).
  function(where = .externalCallerEnv(), inherits = missing(where))
{
    pat <- paste0("^",classMetaName(""))
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
    substring(clNames, nchar(pat, "c"))
}


validObject <- function(object, test = FALSE, complete = FALSE)
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
    attrNames <- c(".Data", ".S3Class", names(attributes(object)))
    if(any(is.na(match(slotNames, attrNames)))) {
        badSlots <- is.na(match(slotNames, attrNames))
	errors <-
	    c(errors,
	      paste("slots in class definition but not in object:",
		    paste0('"', slotNames[badSlots], '"', collapse = ", ")))
        slotTypes <- slotTypes[!badSlots]
        slotNames <- slotNames[!badSlots]
    }
    for(i in seq_along(slotTypes)) {
	classi <- slotTypes[[i]]
	classDefi <- getClassDef(classi, where = where)
	if(is.null(classDefi)) {
	    errors <- c(errors,
			paste0("undefined class for slot \"", slotNames[[i]],
			       "\" (\"", classi, "\")"))
	    next
	}
        namei <- slotNames[[i]]
        sloti <- try(switch(namei,
                            ## .S3Class for S3 objects (e.g., "factor")
                            .S3Class = S3Class(object),
                            slot(object, namei)
                            ), silent = TRUE)
        if(inherits(sloti, "try-error")) {
           errors <- c(errors, sloti)
           next
        }
	## note that the use of possibleExtends is shared with checkSlotAssignment(), in case a
	## future revision improves on it!
	ok <- possibleExtends(class(sloti), classi, ClassDef2 = classDefi)
	if(identical(ok, FALSE)) {
	    errors <- c(errors,
			paste0("invalid object for slot \"", slotNames[[i]],
			       "\" in class \"", Class,
			       "\": got class \"", class(sloti),
			       "\", should be or extend class \"", classi, "\""))
	    next
	}
	if(!complete)
          next
        errori <- anyStrings(Recall(sloti, TRUE, TRUE))
        if(length(errori)) {
	    errori <- paste0("In slot \"", slotNames[[i]],
			     "\" of class \"", class(sloti), "\": ", errori)
            errors <- c(errors, errori)
        }
    }
    extends <- rev(classDef@contains)
    for(i in seq_along(extends)) {
	exti <- extends[[i]]
	superClass <- exti@superClass
	if(!exti@simple && !is(object, superClass))
	    next ## skip conditional relations that don't hold for this object
	superDef <- getClassDef(superClass, where = where)
	if(is.null(superDef)) {
	    errors <- c(errors,
			paste0("superclass \"", superClass,
			       "\" not defined in the environment of the object's class"))
	    break
	}
	validityMethod <- superDef@validity
	if(is(validityMethod, "function")) {
	    errors <- c(errors, anyStrings(validityMethod(as(object, superClass))))
	    if(length(errors))
		break
	}
    }
    validityMethod <- classDef@validity
    if(length(errors) == 0L && is(validityMethod, "function")) {
	errors <- c(errors, anyStrings(validityMethod(object)))
    }
    if(length(errors)) {
	if(test)
	    errors
	else {
	    msg <- gettextf("invalid class %s object", dQuote(Class))
	    if(length(errors) > 1L)
		stop(paste(paste0(msg, ":"),
                           paste(seq_along(errors), errors, sep=": "),
			   collapse = "\n"), domain = NA)
	    else stop(msg, ": ", errors, domain = NA)
	}
    }
    else
	TRUE
}

setValidity <- function(Class, method, where = topenv(parent.frame())) {
    if(isClassDef(Class)) {
	ClassDef <- Class
	Class <- ClassDef@className
    }
    else {
	ClassDef <- getClassDef(Class, where)
    }
    method <- .makeValidityMethod(Class, method)
    if(is.null(method) ||
       (is(method, "function") && length(formalArgs(method)) == 1L))
	ClassDef@validity <- method
    else
	stop("validity method must be NULL or a function of one argument")
    ## TO DO:  check the where argument against the package of the class def.
    assignClassDef(Class, ClassDef, where = where)
    resetClass(Class, ClassDef, where = where)
}

getValidity <- function (ClassDef) {
    ## "needed" according to ../man/validObject.Rd
    ClassDef@validity
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
                    where <- findClass(Class, unique = "resetting the definition")[[1L]]
                else
                    where <- .classDefEnv(classDef)
            }
            if(missing(classDef)) {
                classDef <- getClassDef(Class, where)
                if(is.null(classDef)) {
                    warning(gettextf("class %s not found on %s; 'resetClass' will have no effect",
                                     dQuote(Class),
                                     sQuote(getPackageName(where))),
                            domain = NA)
                    return(classDef)
                }
            }
            else if(!is(classDef, "classRepresentation"))
                stop(gettextf("argument 'classDef' must be a string or a class representation; got an object of class %s",
                              dQuote(class(classDef))),
                     domain = NA)
#            package <- getPackageName(where)
        }
        if(classDef@sealed)
            warning(gettextf("class %s is sealed; 'resetClass' will have no effect",
                             dQuote(Class)),
                    domain = NA)
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
    if(length(args)) {
        Class <- class(.Object)
        ## the basic classes have fixed definitions
        if(!is.na(match(Class, .BasicClasses)))
            return(newBasic(Class, ...))
        ClassDef <- getClass(Class)
        ## separate the slots, superclass objects
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        supers <- args[!which]
        thisExtends <- names(ClassDef@contains)
        slotDefs <- ClassDef@slots
        dataPart <- elNamed(slotDefs, ".Data")
        if(is.null(dataPart)) dataPart <- "missing"
        if(length(supers)) {
            for(i in rev(seq_along(supers))) {
                obj <- el(supers, i)
                Classi <- class(obj)
                if(length(Classi) > 1L)
                    Classi <- Classi[[1L]] #possible S3 inheritance
                ## test some cases that let information be copied into the
                ## object, ordered from more to less:  all the slots in the
                ## first two cases, some in the 3rd, just the data part in 4th
                if(.identC(Classi, Class))
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
                    extendsi <- extends(Classi)[-1L]
                    ## look for the common extensions, choose the first
                    ## one in the extensions of Class
                    which <- match(thisExtends, extendsi)
                    which <- seq_along(which)[!is.na(which)]
                    if(length(which)) {
                        Classi <- thisExtends[which[1L]]
###                    was:    as(.Object, Classi) <- as(obj, Classi, strict = FALSE)
                        ## but   as<- does an as(....) to its value argument
                        as(.Object, Classi) <- obj
                    }
                    else
                        stop(gettextf("cannot use object of class %s in new():  class %s does not extend that class",
                                      dQuote(Classi),
                                      dQuote(Class)),
                             domain = NA)
                }
            }
        }
        if(length(elements)) {
            snames <- names(elements)
	    if(anyDuplicated(snames))
                stop(gettextf("duplicated slot names: %s",
                              paste(sQuote(snames[duplicated(snames)]),
                                    collapse = ", ")), domain = NA)
            which  <- match(snames, names(slotDefs))
            if(anyNA(which))
                stop(sprintf(ngettext(sum(is.na(which)),
                                      "invalid name for slot of class %s: %s",
                                      "invalid names for slots of class %s: %s"),
                              dQuote(Class),
                              paste(snames[is.na(which)], collapse=", ")),
                     domain = NA)
            firstTime <- TRUE
            for(i in seq_along(snames)) {
                slotName <- el(snames, i)
                slotClass <- elNamed(slotDefs, slotName)
                slotClassDef <- getClassDef(slotClass, package = ClassDef@package)
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
                if (firstTime) {
                    ## force a copy of .Object
                    slot(.Object, slotName, check = FALSE) <- slotVal
                    firstTime <- FALSE
                } else {
                    ## XXX: do the assignment in-place
                    "slot<-"(.Object, slotName, check = FALSE, slotVal)
                }
            }
        }
        validObject(.Object)
     }
    .Object
}

findClass <- function(Class, where = topenv(parent.frame()), unique = "") {
    if(is(Class, "classRepresentation")) {
        pkg <- Class@package
        classDef <- Class
        Class <- Class@className
    }
    else {
        pkg <- packageSlot(Class)
        if(is.null(pkg))
	    pkg <- ""
        classDef <- getClassDef(Class, where, pkg)
    }
    where <- if(missing(where) && nzchar(pkg)) .requirePackage(pkg) else as.environment(where)
    what <- classMetaName(Class)
    where <- .findAll(what, where)
    if(length(where) > 1L && nzchar(pkg)) {
        pkgs <- sapply(where, function(db)get(what, db)@package)
        where <- where[match(pkg, pkgs, 0L)]
    }
    else
      pkgs <- pkg
    if(length(where) == 0L) {
        if(is.null(classDef))
            classDef <- getClassDef(Class) # but won't likely succeed over previous
        if(nzchar(unique)) {
            if(is(classDef, "classRepresentation"))
                stop(gettextf("class %s is defined, with package %s, but no corresponding metadata object was found (not exported?)",
                              dQuote(Class),
                              sQuote(classDef@package)),
                     domain = NA)
            else
                stop(gettextf("no definition of %s to use for %s",
                              dQuote(Class),
                              unique),
                     domain = NA)
        }
    }
    else if(length(where) > 1L) {
        pkgs <- sapply(where, getPackageName, create = FALSE)
        ## not all environments need be packages (e.g., imports)
        ## We only try to eliminate duplicate package namespaces
        where <- where[!(nzchar(pkgs) & duplicated(pkgs))]
        if(length(where) > 1L)
            if(nzchar(unique)) {
                pkgs <- base::unique(pkgs)
                where <- where[1L]
                ## problem: 'unique'x is text passed in, so do not translate
                warning(sprintf(ngettext(length(pkgs),
                                         "multiple definition of class %s visible (%s); using the definition\n   in package %s for %s",
                                         "multiple definitions of class %s visible (%s); using the definition\n   in package %s for %s"),
                                dQuote(Class),
                                paste(sQuote(pkgs), collapse = ", "),
                                sQuote(pkgs[[1L]]),
                                unique),
                        domain = NA)
            }
            ## else returns a list of >1 places, for the caller to sort out (e.g., .findOrCopyClass)
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

## see $RHOME/src/main/duplicate.c for the corresponding datatypes
## not copied by duplicate1
.AbnormalTypes <- c("environment", "name", "externalptr",  "NULL")


.indirectAbnormalClasses <- paste0(".", .AbnormalTypes)
names(.indirectAbnormalClasses) <- .AbnormalTypes

## the types not supported by indirect classes (yet)
.AbnormalTypes <- c(.AbnormalTypes,
                    "special","builtin", "weakref", "bytecode")

.addAbnormalDataType <- function(classes) {
  types <- match(classes, .AbnormalTypes, 0) > 0
  type = classes[types]
  if(length(type) == 0)
    return(classes)
  if(length(type) > 1)
    stop(gettextf("class definition cannot extend more than one of these data types: %s",
		  paste0('"',type, '"', collapse = ", ")),
         domain = NA)
  class <- .indirectAbnormalClasses[type]
  if(is.na(class))
    stop(gettextf("abnormal type %s is not supported as a superclass of a class definition",
                  dQuote(type)),
         domain = NA)
  ## this message USED TO BE PRINTED: reminds programmers that
  ## they will see an unexpected superclass
  ## message(gettextf('Defining type "%s" as a superclass via class "%s"',
  ##                 type, class), domain = NA)
  c(class, classes[!types])
}

.checkRequiredGenerics <- function(Class, classDef, where) {}

..checkRequiredGenerics <- function(Class, classDef, where) {
  ## If any of the superclasses are in the .NeedPrimitiveMethods
  ## list, cache the corresponding generics now and also save their names in
  ## .requireCachedGenerics to be used when the environment
  ## where= is loaded.
  supers <- names(classDef@contains)
  allNeeded <- get(".NeedPrimitiveMethods", envir = .methodsNamespace)
  specials <- names(allNeeded)
  needed <- match(specials, supers, 0L) > 0L
  if(any(needed)) {
    generics <- unique(allNeeded[needed])
    packages <- vapply(generics, function(g) {
        def <- getGeneric(g)
        pkg <- def@package # must be "methods" ?
        cacheGenericsMetaData(g, def, TRUE, where, pkg)
        pkg
    }, character(1))
    previous <- if(exists(".requireCachedGenerics", where, inherits = FALSE))
		      get(".requireCachedGenerics", where) else character()
    packages <- c(attr(previous, "package"), packages)
    gg <- c(previous, generics)
    attr(gg, "package") <- packages
    assign(".requireCachedGenerics", gg, where)
  }
}

.setS3MethodsOn <- function(classDef) {
    ext <- extends(classDef)
    slots <- classDef@slots
    if(is.na(match(".S3Class", names(slots)))) {
        ## add the slot if it's not there
        slots$.S3Class <- getClass("oldClass")@slots$.S3Class
        classDef@slots <- slots
    }
    ## in any case give the prototype the full extends as .S3Class
    proto <- classDef@prototype
    if(is.null(proto)) # simple virtual class--unlikely but valid
        proto <- defaultPrototype()
    attr(proto, ".S3Class") <- ext
    classDef@prototype <- proto
    classDef
  }

multipleClasses <- function(details = FALSE) {
    classes <- as.list(.classTable, all.names=TRUE)
    dups <- Filter(is.list, classes)
    if(details) dups else names(dups)
}

className <- function(class, package) {
    if(is(class, "character")) {
        className <- as.character(class)
        if(missing(package))
            package <- packageSlot(class)
        if(is.null(package)) {
            if(exists(className, envir = .classTable, inherits = FALSE))
                classDef <- get(className, envir = .classTable)
            else {
                classDef <- findClass(className, topenv(parent.frame()))
                if(length(classDef) == 1)
                    classDef <- classDef[[1]]
            }
            ## at this point, classDef is the definition if
            ## unique, otherwise a list of 0 or >1 definitions
            if(is(classDef, "classRepresentation"))
                package <- classDef@package
            else if(length(classDef) > 1L) {
                pkgs <- sapply(classDef, function(cl)cl@package)
                warning(gettextf("multiple class definitions for %s from packages: %s; picking the first",
                                 dQuote(className),
                                 paste(sQuote(pkgs), collapse = ", ")),
                        domain = NA)
                package <- pkgs[[1L]]
            }
            else
                stop(gettextf("no package name supplied and no class definition found for %s",
                              dQuote(className)),
                     domain = NA)
        }
    }
    else if(is(class, classDef)) {
        className <- class@className
        if(missing(package))
            package <- class@package
    }
    new("className", .Data = className, package = package)
}

## bootstrap version before the class is defined
classGeneratorFunction <- function(classDef, env = topenv(parent.frame())) {
    fun <- function(...)NULL
    ## put the class name with package attribute into new()
    body(fun) <- substitute(new(CLASS, ...),
                            list(CLASS = classDef@className))
    environment(fun) <- env
    fun
}

.classGeneratorFunction <- function(classDef, env = topenv(parent.frame())) {
    if(is(classDef, "classRepresentation")) {}
    else if(is(classDef, "character")) {
        if(is.null(packageSlot(classDef)))
            classDef <- getClass(classDef, where = env)
        else
            classDef <- getClass(classDef)
    }
    else
        stop("argument 'classDef' must be a class definition or the name of a class")
    fun <- function(...)NULL
    ## put the class name with package attribute into new()
    body(fun) <- substitute(new(CLASS, ...),
                            list(CLASS = classDef@className))
    environment(fun) <- env
    fun <- as(fun, "classGeneratorFunction")
    fun@className <- classDef@className
    fun@package <- classDef@package
    fun
}

## grammar: 'what' is an adjective, so not plural ....
inferProperties <- function(props, what) {
    .validPropNames <- function(propNames) {
        n <- length(props)
        if(!n)
            return(character())
        else if(is.null(propNames))
            stop(gettextf("No %s names supplied", what),
                 domain = NA, call. = FALSE)
        else if(!all(nzchar(propNames)))
            stop(gettextf("All %s names must be nonempty in:\n(%s)", what,
                          paste(sQuote(propNames), collapse = ", ")),
                 domain = NA, call. = FALSE)
        else if(any(duplicated(propNames))) # NB: not translatable because of plurals
            stop(gettextf("All %s names must be distinct in:\n(%s)", what,
                          paste(sQuote(propNames), collapse = ", ")),
                 domain = NA, call. = FALSE)
        propNames
    }
    if(is.character(props)) {
        propNames <- names(props)
        if(is.null(propNames)) {
            propNames <- .validPropNames(props) # the text is the names
            ## treat as "ANY"
            props <- as.list(rep("ANY", length(props)))
            names(props) <- propNames
        }
        else {
            .validPropNames(propNames)
            props <- as.list(props)
        }
    }
    else if(is.list(props)) {
        if(length(props) > 0) # just validate them
            .validPropNames(names(props))
    }
    else
        stop(gettextf("argument %s must be a list or a character vector; got an object of class %s",
                      dQuote(what), dQuote(class(fields))),
             domain = NA)
    props
}


