#  File src/library/methods/R/RClassUtils.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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
#  http://www.r-project.org/Licenses/

testVirtual <-
  ## Test for a Virtual Class.
  ## Figures out, as well as possible, whether the class with these properties,
  ## extension, and prototype is a virtual class.
  ## Can be forced to be virtual by extending "VIRTUAL".  Otherwise, a class is
  ## virtual only if it has no slots, extends no non-virtual classes, and has a
  ## NULL Prototype
  function(properties, extends, prototype, where)
{
    if(length(extends)) {
        en <- names(extends)
        if(!is.na(match("VIRTUAL", en)))
            return(TRUE)
        ## does the class extend a known non-virtual class?
        for(what in en) {
            enDef <- getClassDef(what, where)
            if(!is.null(enDef) && identical(enDef@virtual, FALSE))
                return(FALSE)
        }
    }
    (length(properties) == 0L && is.null(prototype))
}

makePrototypeFromClassDef <-
  ## completes the prototype implied by
  ## the class definition.
  ##
  ##  The following three rules are applied in this order.
  ##
  ## If the class has slots, then the prototype for each
  ## slot is used by default, but a corresponding element in the explicitly supplied
  ## prototype, if there is one, is used instead (but it must be coercible to the
  ## class of the slot).
  ##
  ## If there are no slots but a non-null prototype was specified, this is returned.
  ##
  ## If there is a single non-virtual superclass (a class in the extends list),
  ## then its prototype is used.
  ##
  ## If all three of the above fail, the prototype is `NULL'.
  function(slots, ClassDef, extends, where)
{
    className <- ClassDef@className
    snames <- names(slots)
    ## try for a single superclass that is not virtual
    supers <- names(extends)
    virtual <- NA
    dataPartClass <- elNamed(slots, ".Data")
    prototype <- ClassDef@prototype
    dataPartDone <- is.null(dataPartClass)  || is(prototype, dataPartClass)# don't look for data part in supreclasses
    ## check for a formal prototype object (TODO:  sometime ensure that this happens
    ## at setClass() time, so prototype slot in classRepresentation can have that class
    if(!.identC(class(prototype), className) && .isPrototype(prototype)) {
        pnames <- prototype@slots
        prototype <- prototype@object
    }
    else
        pnames <- names(attributes(prototype))
    if(length(slots) == 0L && !is.null(prototype))
            return(prototype)
    for(i in seq_along(extends)) {
        what <- el(supers, i)
        exti <- extends[[i]]
        if(identical(exti@simple, FALSE))
            next ## only simple contains rel'ns give slots
        if(identical(what, "VIRTUAL"))
            ## the class is virtual, and the prototype usually NULL
            virtual <- TRUE
        else if(isClass(what, where = where)) {
            cli <- getClass(what, where = where)
            slotsi <- names(cli@slots)
            pri <- cli@prototype
            ## once in a while
            if(is.null(prototype)) {
                prototype <- pri
                pnames <- names(attributes(prototype))
                fromClass <- what
            }
            else if(length(slots)) {
                for(slotName in slotsi) {
                    if(identical(slotName, ".Data")) {
                        if(!dataPartDone) {
                            prototype <- setDataPart(prototype, getDataPart(pri), FALSE)
                            dataPartDone <- TRUE
                        }
                    }
                    else if(is.na(match(slotName, pnames))) {
                        ## possible that the prototype already had this slot specified
                        ## If not, add it now.
                        attr(prototype, slotName) <- attr(pri, slotName)
                        pnames <- c(pnames, slotName)
                    }
                }
            }
            else if(!dataPartDone && extends(cli, dataPartClass)) {
                 prototype <- setDataPart(prototype, pri, FALSE)
                 dataPartDone <- TRUE
            }
        }
    }
    if(length(slots) == 0L)
        return(prototype)
    if(is.null(prototype))
        prototype <- defaultPrototype()
    pnames <- names(attributes(prototype))
    ## watch out for a prototype of this class.  Not supposed to happen, but will
    ## at least for the basic class "ts", and can lead to inf. recursion
    pslots <-
        if(.identC(class(prototype), className))
            names(attributes(unclass(prototype)))
        else if(isClass(class(prototype)))
            names(getSlots(getClass(class(prototype))))
        ## else NULL

    ## now check that all the directly specified slots have corresponding elements
    ## in the prototype--the inherited slots were done in the loop over extends
    if(!is.na(match(".Data", snames))) {
        dataPartClass <- elNamed(slots, ".Data")

        ## check the data part
        if(!(isVirtualClass(dataPartClass))) {
            if(isClass(class(prototype), where = where)) {
                prototypeClass <- getClass(class(prototype), where = where)
                OK <- extends(prototypeClass, dataPartClass)
            }
            else
                OK <- FALSE
            if(identical(OK, FALSE))
                stop(gettextf("in constructing the prototype for class %s: prototype has class %s, but the data part specifies class %s",
                              dQuote(className),
                              dQuote(.class1(prototype)),
                              dQuote(dataPartClass)),
                     domain = NA)
        }
        iData <- -match(".Data", snames)
        snames <- snames[iData]
        slots <- slots[iData]
    }
    for(j in seq_along(snames)) {
        name <- el(snames, j)
        i <- match(name, pnames)
        if(is.na(i)) {
            ## if the class of the j-th element of slots is defined and non-virtual,
            ## generate an object from it; else insert NULL
            slot(prototype, name, check = FALSE) <- tryNew(el(slots, j), where)
        }
    }
    extra <- pnames[is.na(match(pnames, snames)) & !is.na(match(pnames, pslots))]
    if(length(extra) && is.na(match("oldClass", supers)))
        warning(gettextf("in constructing the prototype for class %s, slots in prototype and not in class: %s",
                         dQuote(className),
                         paste(extra, collapse=", ")),
                domain = NA)
    ## now check the elements of the prototype against the class definition
    slotDefs <- getSlots(ClassDef); slotNames <- names(slotDefs)
    pnames <- names(attributes(prototype))
    pnames <- pnames[!is.na(match(pnames, slotNames))]
    check <- rep.int(FALSE, length(pnames))
    for(what in pnames) {
        pwhat <- slot(prototype, what)
        slotClass <- getClassDef(slotDefs[[what]], where)
        if(is.null(slotClass) || !extends(class(pwhat), slotClass)) {
            if(is.null(pwhat)) { # does this still apply??
            }
            else if(is(slotClass, "classRepresentation") &&
                    slotClass@virtual) {} # no nonvirtual prototype;e.g. S3 class
            else
                check[match(what, pnames)] <- TRUE
        }
    }
    if(any(check))
        stop(gettextf("in making the prototype for class %s elements of the prototype failed to match the corresponding slot class: %s",
                      dQuote(className),
                      paste(pnames[check],
                            "(class",
                            .dQ(slotDefs[match(pnames[check], slotNames)]),
                            ")",
                            collapse = ", ")),
             domain = NA)
    prototype
}

newEmptyObject <-
  ## Utility function to create an empty object into which slots can be
  ## set.  Currently just creates an empty list with class "NULL"
  ##
  ## Later version should create a special object reference that marks an
  ## object currently with no slots and no data.
  function()
{
    value <- list()
    value
}


completeClassDefinition <-
  ## Completes the definition of Class, relative to the current environment
  ##
  ## The completed definition is stored in the session's class metadata,
  ## to be retrieved the next time that getClass is called on this class,
  ## and is returned as the value of the call.
  function(Class, ClassDef = getClassDef(Class), where, doExtends = TRUE)
{
    ClassDef <- .completeClassSlots(ClassDef, where)
    immediate <- ClassDef@contains
    properties <- ClassDef@slots
    prototype <- makePrototypeFromClassDef(properties, ClassDef, immediate, where)
    virtual <- ClassDef@virtual
    validity <- ClassDef@validity
    access <- ClassDef@access
    package <- ClassDef@package
    extends    <- if(doExtends) completeExtends   (ClassDef, where = where) else ClassDef@contains
    subclasses <- if(doExtends) completeSubclasses(ClassDef, where = where) else ClassDef@subclasses
    if(is.na(virtual))
        ## compute it from the immediate extensions, but all the properties
        virtual <- testVirtual(properties, immediate, prototype, where)
    ## modify the initial class definition object, rather than creating
    ## a new one, to allow extensions of "classRepresentation"
    ## Done by a separate function to allow a bootstrap version.
    ClassDef <- .mergeClassDefSlots(ClassDef,
                                    slots = properties,
                                    contains = extends,
                                    prototype = prototype,
                                    virtual = virtual,
                                    subclasses = subclasses)
    if(any(!is.na(match(names(ClassDef@subclasses), names(ClassDef@contains))))
       && getOption("warn") > 0 ## NEEDED:  a better way to turn on strict testing
       ) {
        bad <- names(ClassDef@subclasses)[!is.na(match(names(ClassDef@subclasses), names(ClassDef@contains)))]
        warning(gettextf("potential cycle in class inheritance: %s has duplicates in superclasses and subclasses (%s)",
                         dQuote(Class),
                         paste(bad, collapse = ", ")),
                domain = NA)
    }
    ClassDef
}

.completeClassSlots <- function(ClassDef, where) {
        properties <- ClassDef@slots
        simpleContains <- ClassDef@contains
        Class <- ClassDef@className
        package <- ClassDef@package
        ext <- getAllSuperClasses(ClassDef, TRUE)
        ## ext has the names of all the direct and indirect superClasses but NOT those that do
        ## an explicit coerce (we can't conclude anything about slots, etc. from them)
        if(length(ext)) {
            superProps <- vector("list", length(ext)+1L)
            superProps[[1L]] <- properties
            for(i in seq_along(ext)) {
                eClass <- ext[[i]]
                if(isClass(eClass, where = where))
                    superProps[[i+1]] <- getClassDef(eClass, where = where)@slots
            }
            properties <- unlist(superProps, recursive = FALSE)
            ## check for conflicting slot names
            if(anyDuplicated(allNames(properties))) {
                duped <- duplicated(names(properties))
#TEMPORARY -- until classes are completed in place & we have way to match non-inherited slots
                properties <- properties[!duped]
#                 dupNames <- unique(names(properties)[duped])
#                 if(!is.na(match(".Data", dupNames))) {
#                     dataParts <- seq_along(properties)[names(properties) == ".Data"]
#                     dupNames <- dupNames[dupNames != ".Data"]
#                     ## inherited data part classes are OK but should be consistent
#                     dataPartClasses <- unique(as.character(properties[dataParts]))
#                     if(length(dataPartClasses)>1)
#                         warning("Inconsistent data part classes inherited (",
#                                 paste(dataPartClasses, collapse = ", "),
#                                 "): coercion to some may fail")
#                     ## remove all but the first .Data
#                     properties <- properties[-dataParts[-1L]]
#                 }
#                 if(length(dupNames)>0) {
#                     dupClasses <- logical(length(superProps))
#                     for(i in seq_along(superProps)) {
#                         dupClasses[i] <- !all(is.na(match(dupNames, names(superProps[[i]]))))
#                     }
#                     stop(paste("Duplicate slot names: slots ",
#                                paste(dupNames, collapse =", "), "; see classes ",
#                                paste0(c(Class, ext)[dupClasses], collapse = ", ")))
#                }
            }
        }
        ## ensure that each element of the slots is a valid class reference
        undefClasses <- rep.int(FALSE, length(properties))
        for(i in seq_along(properties)) {
            cli <- properties[[i]]
            if(is.null(packageSlot(cli))) {
                cliDef <- getClassDef(cli, where)
                if(is.null(cliDef))
                    undefClasses[[i]] <- TRUE
                else
                    packageSlot(properties[[i]]) <- cliDef@package
            }
            else {
                cliDef <- getClassDef(cli)
                if(is.null(cliDef))
                    undefClasses[[i]] <- TRUE
            }
        }
        if(any(undefClasses))
            warning(gettextf("undefined slot classes in definition of %s: %s",
                             .dQ(ClassDef@className),
                             paste(names(properties)[undefClasses], "(class ",
                                   .dQ(unlist(properties, recursive = FALSE)[undefClasses]),
                                   ")", collapse = ", ", sep = "")),
                    call. = FALSE, domain = NA)
        ClassDef@slots <- properties
        ClassDef
}

.uncompleteClassDefinition <- function(ClassDef, slotName) {
    if(missing(slotName)) {
        ClassDef <- Recall(ClassDef, "contains")
        Recall(ClassDef, "subclasses")
    }
    else {
        prev <- slot(ClassDef, slotName)
        if(length(prev)) {
            indir <- sapply(prev, .isIndirectExtension)
            slot(ClassDef, slotName) <- slot(ClassDef, slotName)[!indir]
        }
        ClassDef
    }
}

.isIndirectExtension <- function(object) {
    is(object, "SClassExtension") && length(object@by) > 0
}

.mergeSlots <- function(classDef1, classDef2) {

}

.directSubClasses <- function(ClassDef) {
    ## no checks for input here:
    if(length(sc <- ClassDef@subclasses)) {
        names(sc)[sapply(sc, function(cc) cc@distance == 1L)]
    } ## else NULL
}

getAllSuperClasses <-
  ## Get the names of all the classes that this class definition extends.
  ##
  ## A utility function used to complete a class definition.  It
  ## returns all the superclasses reachable from this class, in
  ## depth-first order (which is the order used for matching methods);
  ## that is, the first direct superclass followed by all its
  ## superclasses, then the next, etc.  (The order is relevant only in
  ## the case that some of the superclasses have multiple inheritance.)
  ##
  ## The list of superclasses is stored in the extends property of the
  ## session metadata.  User code should not need to call
  ## getAllSuperClasses directly; instead, use getClass()@contains
  ## (which will complete the definition if necessary).
  function(ClassDef, simpleOnly = TRUE) {
    temp <- superClassDepth(ClassDef, simpleOnly = simpleOnly)
    unique(temp$label[sort.list(temp$depth)])
  }

superClassDepth <-
    ## all the superclasses of ClassDef, along with the depth of the relation
    ## Includes the extension definitions, but these are not currently used by
    ## getAllSuperClasses
  function(ClassDef, soFar = ClassDef@className, simpleOnly = TRUE)
{
    ext <- ClassDef@contains
    ## remove indirect and maybe non-simple superclasses (latter for inferring slots)
    ok <- rep.int(TRUE, length(ext))
    for(i in seq_along(ext)) {
        exti <- ext[[i]]
        if(.isIndirectExtension(exti) ||
           (simpleOnly && ! exti @simple))
            ok[i] <- FALSE
    }
    ext <- ext[ok]
    immediate <- names(ext)
    notSoFar <- is.na(match(immediate, soFar))
    immediate <- immediate[notSoFar]
    super <- list(label = immediate, depth = rep.int(1, length(immediate)),
                  ext = ext)
    for(i in seq_along(immediate)) {
        what <- immediate[[i]]
        if(!is.na(match(what, soFar)))
           ## watch out for loops (e.g., matrix/array have mutual is relationship)
           next
        exti <- ext[[i]]
        soFar <- c(soFar, what)
        if(!is(exti, "SClassExtension"))
            stop(gettextf("in definition of class %s, information for superclass %s is of class %s (expected \"SClassExtension\")",
                          dQuote(ClassDef@className),
                          dQuote(what),
                          dQuote(class(exti))),
                 domain = NA)
        superClass <-  getClassDef(exti@superClass, package = exti@package)
            if(is.null(superClass)) {
                warning(gettextf("class %s extends an undefined class, %s",
                                 dQuote(ClassDef@className),
                                 dQuote(what)),
                        domain = NA)
                next
            }
            more <- Recall(superClass, soFar)
            whatMore <- more$label
            if(!all(is.na(match(whatMore, soFar)))) {
                ## elminate classes reachable by more than one path
                ## (This is allowed in the model, however)
                ok <- is.na(match(whatMore, soFar))
                more$depth <- more$depth[ok]
                more$label <- more$label[ok]
                more$ext <- more$ext[ok]
                whatMore <- whatMore[ok]
            }
            if(length(whatMore)) {
                soFar <- c(soFar, whatMore)
                super$depth <- c(super$depth, 1+more$depth)
                super$label <- c(super$label, more$label)
                super$ext <- c(super$ext, more$ext)
            }
    }
    super
}

selectSuperClasses <-
    function(Class, dropVirtual = FALSE, namesOnly = TRUE,
             directOnly = TRUE, simpleOnly = directOnly,
             where = topenv(parent.frame()))
{
    ext <- if(isClassDef(Class))
        Class@contains
    else if(isClass(Class, where = where))
        getClass(Class, where = where)@contains
    else stop("'Class' must be a valid class definition or class")

    .selectSuperClasses(ext, dropVirtual = dropVirtual, namesOnly = namesOnly,
                        directOnly = directOnly, simpleOnly = simpleOnly)
}

.selectSuperClasses <- function(ext, dropVirtual = FALSE, namesOnly = TRUE,
                                directOnly = TRUE, simpleOnly = directOnly)
{
    ## No argument checking here
    addCond <- function(xpr, prev)
        if(length(prev)) substitute(P && N, list(P = prev, N = xpr)) else xpr
    C <- if(dropVirtual) {
        ## NB the default 'where' in getClass() may depend on specific superClass:
        isVirtualExt <- function(x) getClass(x@superClass)@virtual
        quote(!isVirtualExt(exti))
    } else expression()
    if(directOnly) C <- addCond(quote(length(exti@by) == 0), C)
    if(simpleOnly) C <- addCond(quote(exti@simple), C)
    if(length(C)) {
      F <- function(exti){}; body(F) <- C
      ext <- ext[unlist(lapply(ext, F), use.names=FALSE)]
    }
    if(namesOnly) names(ext) else ext
}

inheritedSlotNames <- function(Class, where = topenv(parent.frame()))
{
    ext <- if(isClassDef(Class))
        Class@contains
    else if(isClass(Class, where = where))
        getClass(Class, where = where)@contains
    supcl <- .selectSuperClasses(ext) ## maybe  simpleOnly = FALSE or use as argument?
    unique(unlist(lapply(lapply(supcl, getClassDef), slotNames), use.names=FALSE))
    ## or just the non-simplified part (*with* names):
    ##     lapply(sapply(supcl, getClassDef, simplify=FALSE), slotNames)
}


isVirtualClass <-
  ## Is the named class a virtual class?  A class is virtual if explicitly declared to
  ## be, and also if the class is not formally defined.
  function(Class, where = topenv(parent.frame())) {
      if(isClassDef(Class))
          Class@virtual
      else if(isClass(Class, where = where))
          getClass(Class, where = where)@virtual
      else
          TRUE
  }


assignClassDef <-
  ## assign the definition of the class to the specially named object
  function(Class, def, where = .GlobalEnv, force = FALSE) {
      if(!is(def,"classRepresentation"))
          stop(gettextf("trying to assign an object of class %s as the definition of class %s: must supply a \"classRepresentation\" object",
                        dQuote(class(def)),
                        dQuote(Class)),
               domain = NA)
      clName <- def@className; attributes(clName) <- NULL
      if(!.identC(Class, clName))
          stop(gettextf("assigning as %s a class representation with internal name %s",
                        dQuote(Class),
                        dQuote(def@className)),
               domain = NA)
      where <- as.environment(where)
      mname <- classMetaName(Class)
      if(exists(mname, envir = where, inherits = FALSE) && bindingIsLocked(mname, where)) {
          if(force)
            .assignOverBinding(mname, def, where, FALSE)
          ## called this way, e.g., from setIs()
          ## This is old and bad.  Given that the cached version of the class
          ## will have all the updated info about a class, we should leave
          ## the locked version alone.  But probably too late to fix without
          ## a lot of flack.  (JMC, 2013/10)
          else
            stop(gettextf("class %s has a locked definition in package %s",
                          dQuote(Class), sQuote(getPackageName(where))))
      }
      else
          assign(mname, def, where)
      if(cacheOnAssign(where)) # will be FALSE for sourceEnvironment's
          .cacheClass(clName, def, is(def, "ClassUnionRepresentation"), where)
  }


.InitClassDefinition <- function(where) {
    defSlots <- list(slots = "list", contains = "list", virtual = "logical",
                     prototype = "ANY", validity = "OptionalFunction", access = "list",
                     ## the above are to conform to the API; now some extensions
                     className = "character", package = "character",
                     subclasses = "list", versionKey = "externalptr", ## or "integer"??
                     sealed = "logical")
    ## the prototype of a new class def'n:  virtual class with NULL prototype
    protoSlots <- list(slots=list(), contains=list(), virtual=NA,
                  prototype = NULL, validity = NULL,
                  access = list(), className = character(), package = character(),
                  subclasses = list(), versionKey = .newExternalptr(),
                  sealed = FALSE)
    proto <- defaultPrototype()
    pnames <- names(protoSlots)
    for(i in seq_along(protoSlots))
        slot(proto, pnames[[i]], FALSE) <- protoSlots[[i]]
    classRepClass <- .classNameFromMethods("classRepresentation")
    class(proto) <- classRepClass
    object <- defaultPrototype()
    class(object) <- classRepClass
    slot(object, "slots", FALSE) <- defSlots
    slot(object, "className", FALSE) <- classRepClass
    slot(object, "virtual", FALSE) <- FALSE
    slot(object, "prototype", FALSE) <- proto
    for(what in c("contains", "validity", "access", "hasValidity", "subclasses",
                  "versionKey"))
        slot(object, what, FALSE) <- elNamed(protoSlots, what)
    slot(object, "sealed", FALSE) <- TRUE
    slot(object, "package", FALSE) <- getPackageName(where)
##    assignClassDef("classRepresentation", object, where)
    assign(classMetaName("classRepresentation"), object, where)
    ## the list of needed generics, initially empty (see .InitStructureMethods)
    assign(".NeedPrimitiveMethods", list(), where)
}

.classNameFromMethods <- function(what) {
    packageSlot(what) <- "methods"
    what
  }

.initClassSupport <- function(where) {
    setClass("classPrototypeDef", representation(object = "ANY", slots = "character", dataPart = "logical"),
             sealed = TRUE, where = where)
    setClass(".Other", representation(label = "character"),
             sealed = TRUE, where = where)  # nonvirtual, nobody's subclass, see testInheritedMethods
    ## a class and a method for reporting method selection ambiguities
    setClass("MethodSelectionReport",
         representation(generic = "character", allSelections = "character", target = "character", selected = "character", candidates = "list", note = "character"),
             sealed = TRUE, where = where)
    setClass("classGeneratorFunction",
             representation(className = "character", package = "character"),
             contains = "function")
}


newBasic <-
  ## the implementation of the function `new' for basic classes.
  ##
  ## See `new' for the interpretation of the arguments.
  function(Class, ...) {
      msg <- NULL
      value <- switch(Class,
               "NULL" = return(NULL), ## can't set attr's of NULL in R
               "logical" =,
               "numeric" =,
               "character" =,
               "complex" =,
               "integer" =,
               "raw" =,
               "list" =  as.vector(c(...), Class),
               "expression" = eval(substitute(expression(...))),
               "externalptr" = {
                   if(nargs() > 1)
                       stop("'externalptr' objects cannot be initialized from new()")
                   .newExternalptr()
               },
               "single" = as.single(c(...)),
                  ## note on array, matrix:  not possible to be compatible with
                  ## S-Plus on array, unless R allows 0-length .Dim attribute
               "array" = if(!missing(...)) array(...) else structure(numeric(), .Dim =0L),
               "matrix" = if (!missing(...)) matrix(...) else matrix(0, 0L, 0L),
#               "ts" = ts(...),
# break dependence on package stats
	       "ts" = if(!missing(...)) stats::ts(...) else
		      structure(NA, .Tsp = c(1, 1, 1), class = "ts"),

                ## otherwise:
                  {
                      args <- list(...)
                      if(length(args) == 1L && is(args[[1L]], Class)) {
                          value <- as(args[[1L]], Class)
                      }
                      else if(is.na(match(Class, .BasicClasses)))
                          msg <- paste("Calling new() on an undefined and non-basic class (\"",
                               Class, "\")", sep="")
                      else
                          msg <-
                              gettextf("initializing objects from class %s with these arguments is not supported",
                                       dQuote(Class))
                  }
                  )
  if(is.null(msg))
      value
  else
      stop(msg, domain = NA)
}


## this non-exported function turns on or off
## the use of the S4 type as class prototype
.useS4Prototype <- function(on = TRUE, where  = .methodsNamespace) {
    if(on)
     pp <- .Call(C_Rf_allocS4Object)
    else
     pp <-  list()
    .assignOverBinding(".defaultPrototype", where=where, pp, FALSE)
}

defaultPrototype <-
    ## the starting prototype for a non-virtual class
    ## Should someday be a non-vector sexp type
    function()
    .defaultPrototype

reconcilePropertiesAndPrototype <-
  ## makes a list or a structure look like a prototype for the given class.
  ##
  ## Specifically, returns a structure with attributes corresponding to the slot
  ## names in properties and values taken from prototype if they exist there, from
  ## `new(classi)' for the class, `classi' of the slot if that succeeds, and `NULL'
  ## otherwise.
  ##
  ## The prototype may imply slots not in the properties list.  It is not required that
    ## the extends classes be define at this time.  Should it be?
  function(name, properties, prototype, superClasses, where) {
      ## the StandardPrototype should really be a type that doesn't behave like
      ## a vector.  But none of the existing SEXP types work.  Someday ...
      StandardPrototype <- defaultPrototype()
      slots <-  validSlotNames(allNames(properties))
      dataPartClass <- elNamed(properties, ".Data")
      dataPartValue <- FALSE
      if(!is.null(dataPartClass) && is.null(.validDataPartClass(dataPartClass, where)))
          stop(gettextf("in defining class %s, the supplied data part class, %s is not valid (must be a basic class or a virtual class combining basic classes)",
                        dQuote(name), dQuote(dataPartClass)),
               domain = NA)
      prototypeClass <- getClass(class(prototype), where = where)
      if((!is.null(dataPartClass) || length(superClasses))
         && is.na(match("VIRTUAL", superClasses))) {
          ## Look for a data part in the superclasses, either an inherited
          ## .Data slot, or a basic class.  Uses the first possibility, warns of conflicts
          for(cl in superClasses) {
              clDef <- getClassDef(cl, where = where)
              if(is.null(clDef))
                stop(gettextf("no definition was found for superclass %s in the specification of class %s",
                              dQuote(cl), dQuote(name)),
                     domain = NA)
              thisDataPart <-  .validDataPartClass(clDef, where, dataPartClass)
              if(!is.null(thisDataPart)) {
                    dataPartClass <- thisDataPart
                    if(!is.null(clDef@prototype)) {
                      newObject <- clDef@prototype
                      dataPartValue <- TRUE
                    }
                  }
          }
          if(length(dataPartClass)) {
              if(is.na(match(".Data", slots))) {
                  properties <- c(list(".Data"= dataPartClass), properties)
                  slots <- names(properties)
              }
              else if(!extends(elNamed(properties, ".Data"), dataPartClass))
                  stop(gettextf("conflicting definition of data part: .Data = %s, superclass implies %s",
                                dQuote(elNamed(properties, ".Data")),
                                dQuote(dataPartClass)),
                       domain = NA)
              pslots <- NULL
              if(is.null(prototype)) {
                  if(dataPartValue)
                      prototype <- newObject
                  else if(isVirtualClass(dataPartClass, where = where))
                      ## the equivalent of new("vector")
                      prototype <- newBasic("logical")
                  else
                      prototype <- new(dataPartClass)
                  prototypeClass <- getClass(class(prototype), where = where)
              }
              else {
                  if(extends(prototypeClass, "classPrototypeDef")) {
                      hasDataPart <- identical(prototype@dataPart, TRUE)
                      if(!hasDataPart) {
                          if(!dataPartValue) # didn't get a .Data object
                            newObject <- new(dataPartClass)
                          pobject <- prototype@object
                          ## small amount of head-standing to preserve
                          ## any attributes in newObject & not in pobject
                          anames <- names(attributes(pobject))
                          attributes(newObject)[anames] <- attributes(pobject)
                          prototype@object <- newObject
                      }
                      else if(!extends(getClass(class(prototype@object), where = where)
                                       , dataPartClass))
                          stop(gettextf("a prototype object was supplied with object slot of class %s, but the class definition requires an object that is class %s",
                                        dQuote(class(prototype@object)),
                                        dQuote(dataPartClass)),
                               domain = NA)
                  }
                  else if(!extends(prototypeClass, dataPartClass))
                      stop(gettextf("a prototype was supplied of class %s, but the class definition requires an object that is class %s",
                                    dQuote(class(prototype)),
                                    dQuote(dataPartClass)),
                           domain = NA)
              }
          }
          if(is.null(prototype)) { ## non-vector (may extend NULL)
              prototype <- StandardPrototype
          }
      }
      ## check for conflicts in the slots
      allProps <- properties
      for(i in seq_along(superClasses)) {
          cl <- superClasses[[i]]
          clDef <- getClassDef(cl, where)
          if(is(clDef, "classRepresentation")) {
              theseProperties <- getSlots(clDef)
              theseSlots <- names(theseProperties)
              theseSlots <- theseSlots[theseSlots == ".Data"] # handled already
              dups <- !is.na(match(theseSlots, allProps))
              for(dup in theseSlots[dups])
                  if(!extends(elNamed(allProps, dup), elNamed(theseProperties, dup)))
                      stop(gettextf("slot %s in class %s currently defined (or inherited) as \"%s\", conflicts with an inherited definition in class %s",
                                    sQuote(dup),
                                    dQuote(name),
                                    elNamed(allProps, dup),
                                    dQuote(cl)),
                           domain = NA)
              theseSlots <- theseSlots[!dups]
              if(length(theseSlots))
                  allProps[theseSlots] <- theseProperties[theseSlots]
          }
          else
              stop(gettextf("class %s extends an undefined class (%s)",
                            dQuote(name), dQuote(cl)),
                   domain = NA)
      }
      if(is.null(dataPartClass)) {
          if(extends(prototypeClass, "classPrototypeDef"))
          {}
          else {
              if(is.list(prototype))
               prototype <- do.call("prototype", prototype)
              if(is.null(prototype))
                  prototype <- StandardPrototype
          }
      }
      else {
          dataPartDef <- getClass(dataPartClass)
          checkDataPart <- !isXS3Class(dataPartDef)
          if(checkDataPart)
            checkDataPart  <-
              ((is.na(match(dataPartClass, .BasicClasses)) &&
                !isVirtualClass(dataPartDef)) || length(dataPartDef@slots))
          if(checkDataPart)
              stop(gettextf("%s is not eligible to be the data part of another class (must be a basic class or a virtual class with no slots)",
                            dQuote(dataPartClass)),
                   domain = NA)
          if(extends(prototypeClass, "classPrototypeDef"))
          {}
          else if(extends(prototypeClass, dataPartClass)) {
              if(extends(prototypeClass, "list") && length(names(prototype)))
                  warning("prototype is a list with named elements (could be ambiguous):  better to use function prototype() to avoid trouble.")
          }
          else if(is.list(prototype))
              prototype <- do.call("prototype", prototype)
      }
      ## pnames will be the names explicitly defined in the prototype
      if(extends(prototypeClass, "classPrototypeDef")) {
          pnames <- prototype@slots
          prototype <- prototype@object
          if(length(superClasses) == 0L && any(is.na(match(pnames, slots))))
              stop(sprintf(ngettext(sum(is.na(match(pnames, slots))),
                                    "named elements of prototype do not correspond to slot name: %s",
                                    "named elements of prototype do not correspond to slot names: %s"),
                           paste(.dQ(pnames[is.na(match(pnames, slots))]),
                                 collapse =", ")),
                   domain = NA)
      }
      else
          pnames <- allNames(attributes(prototype))
       ## now set the slots not yet in the prototype object.
      ## An important detail is that these are
      ## set using slot<- with check=FALSE (because the slot will not be there already)
      ## what <- is.na(match(slots, pnames))
      what <- seq_along(properties)
      props <- properties[what]
      what <- slots[what]
      nm <- names(attributes(prototype))
      for(i in seq_along(what)) {
          propName <- el(what, i)
          if(!identical(propName, ".Data") && !propName %in% nm)
#             is.null(attr(prototype, propName)))
              slot(prototype, propName, FALSE) <- tryNew(el(props, i), where)
      }
      list(properties = properties, prototype = prototype)
  }

tryNew <-
    ## Tries to generate a new element from this class, but if
    ## the class is undefined just returns NULL.
    ##
    ## For virtual classes, returns the class prototype
    ## so that the object is valid member of class.
    ## Otherwise tries to generate a new() object, but in rare
    ## cases, this might fail if the install() method required
    ## an argument, so this case is trapped as well.
  function(Class, where)
{
    ClassDef <- getClassDef(Class, where)
    if(is.null(ClassDef))
        return(NULL)
    else if(identical(ClassDef@virtual, TRUE))
        ClassDef@prototype
    else tryCatch(new(ClassDef),
                  error = function(e) {
                      value <- ClassDef@prototype
                      class(value) <- ClassDef@className
                      value
                  })
}

empty.dump <- function() list()

isClassDef <- function(object) is(object, "classRepresentation")

showClass <-
    ## print the information about a class definition.
    ## If complete==TRUE, include the indirect information about extensions.
    function(Class, complete = TRUE, propertiesAreCalled = "Slots")
{
    if(isClassDef(Class)) {
        ClassDef <- Class
        Class <- ClassDef@className
    }
    else if(complete)
        ClassDef <- getClass(Class)
    else
        ClassDef <- getClassDef(Class)
    cat(if(identical(ClassDef@virtual, TRUE)) "Virtual ",
	"Class ", .dQ(Class),
	## Show the package if that is non-trivial:
	if(nzchar(pkg <- ClassDef@package))
	c(" [", if(pkg != ".GlobalEnv") "package" else "in", " \"", pkg,"\"]"),
	"\n", sep="")
    x <- ClassDef@slots
    if(length(x)) {
        printPropertiesList(x, propertiesAreCalled)
    }
    else
        cat("\nNo ", propertiesAreCalled, ", prototype of class \"",
            .class1(ClassDef@prototype), "\"\n", sep="")
    ext <- ClassDef@contains
    if(length(ext)) {
        cat("\nExtends: ")
        showExtends(ext)
    }
    ext <- ClassDef@subclasses
    if(length(ext)) {
        cat("\nKnown Subclasses: ")
        showExtends(ext)
    }
}

printPropertiesList <- function(x, propertiesAreCalled) {
    if(length(x)) {
        n <- length(x)
        cat("\n",propertiesAreCalled, ":\n", sep="")
        text <- format(c(names(x), as.character(x)), justify="right")
        text <- matrix(text, nrow = 2L, ncol = n, byrow = TRUE)
        dimnames(text) <- list(c("Name:", "Class:"), rep.int("", n))
        print(text, quote = FALSE)
    }
}

showExtends <-
    ## print the elements of the list of extensions.  Also used to print
    ## extensions recorded in the opposite direction, via a subclass list
    function(ext, printTo = stdout())
{
    what <- names(ext)
    how <- character(length(ext))
    for(i in seq_along(ext)) {
        eli <- el(ext, i)
        if(is(eli, "SClassExtension")) {
            how[i] <-
                if(length(eli@by))
		    paste("by class", paste0("\"", eli@by, "\", distance ",
					     eli@distance, collapse = ", "))
                else if(identical(eli@dataPart, TRUE))
                    "from data part"
                else "directly"
            if(!eli@simple) {
                if(is.function(eli@test) && !identical(body(eli@test), TRUE)) {
                    how[i] <-
                        paste(how[i], if(is.function(eli@coerce))
                              ", with explicit test and coerce" else
                              ", with explicit test", sep="")
                }
                else if(is.function(eli@coerce))
                    how[i] <- paste0(how[i], ", with explicit coerce")
            }
        }
    }
    if(identical(printTo, FALSE))
        list(what = what, how = how)
    else if(all(!nzchar(how)) ||  all(how == "directly")) {
        what <- paste0('"', what, '"')
        if(length(what) > 1L)
            what <- c(paste0(what[-length(what)], ","), what[[length(what)]])
        cat(file = printTo, what, fill=TRUE)
    }
    else cat(file = printTo, "\n",
	     paste0("Class \"", what, "\", ", how, "\n"), sep = "")
}



printClassRepresentation <-
  function(x, ...)
  showClass(x, propertiesAreCalled="Slots")

## bootstrap definition to be used before getClass() works
possibleExtends <- function(class1, class2, ClassDef1, ClassDef2)
    .identC(class1, class2) || .identC(class2, "ANY")

## "Real" definition (assigned in ./zzz.R )
.possibleExtends <-
    ## Find the information that says whether class1 extends class2,
    ## directly or indirectly.  This can be either a logical value or
    ## an object containing various functions to test and/or coerce the relationship.
    ## TODO:  convert into a generic function w. methods WHEN dispatch is really fast!
    function(class1, class2, ClassDef1 = getClassDef(class1),
             ClassDef2 = getClassDef(class2, where = .classEnv(ClassDef1)))
{
    if(.identC(class1[[1L]], class2) || .identC(class2, "ANY"))
        return(TRUE)
    ext <- TRUE # may become a list of extends definitions
    if(is.null(ClassDef1)) # class1 not defined
        return(FALSE)
    ## else
    ext <- ClassDef1@contains
    nm1 <- names(ext)
    i <- match(class2, nm1)
    if(is.na(i)) {
        ## look for class1 in the known subclasses of class2
        if(!is.null(ClassDef2)) {
            ext <- ClassDef2@subclasses
            ## check for a classUnion definition, not a plain "classRepresentation"
            if(!.identC(class(ClassDef2), "classRepresentation") &&
               isClassUnion(ClassDef2))
                ## a simple TRUE iff class1 or one of its superclasses belongs to the union
		i <- as.logical(anyDuplicated(c(class1, unique(nm1),
						names(ext))))
            else {
                ## class1 could be multiple classes here.
                ## I think we want to know if any extend
                i <- match(class1, names(ext))
                ii <- i[!is.na(i)]
                i <- if(length(ii))  ii[1L] else i[1L]
            }
        }
    }
    if(is.na(i))
        FALSE
    else if(is.logical(i))
        i
    else
        el(ext, i)
}

  ## complete the extends information in the class definition, by following
  ## transitive chains.
  ##
  ## Elements in the immediate extends list may be added and current elements may be
  ## replaced, either by replacing a conditional relation with an unconditional
  ## one, or by adding indirect relations.
  ##
completeExtends <-    function(ClassDef, class2, extensionDef, where) {
    ## check for indirect extensions => already completed
    ext <- ClassDef@contains
    for(i in seq_along(ext)) {
        if(.isIndirectExtension(ext[[i]])) {
            ClassDef <- .uncompleteClassDefinition(ClassDef, "contains")
            break
        }
    }
    exts <- .walkClassGraph(ClassDef, "contains", where, attr(ext, "conflicts"))
    if(length(exts)) {
##         ## sort the extends information by depth (required for method dispatch)
##         superClassNames <- getAllSuperClasses(ClassDef, FALSE)
##         ## FIXME:  getAllSuperClassses sometimes misses.  Why?
##         if(length(superClassNames) == length(exts))
##             exts <- exts[superClassNames]
        if("oldClass" %in% names(exts) &&
           length(ClassDef@slots) > 1L) # an extension of an S3 class
          exts <- .S3Extends(ClassDef, exts, where)
    }
    if(!missing(class2) && length(ClassDef@subclasses)) {
        strictBy <- TRUE # FIXME:  would like to make this conditional but a safe condition is unknown
        subclasses <-
            .transitiveSubclasses(ClassDef@className, class2, extensionDef, ClassDef@subclasses, strictBy)
        ## insert the new is relationship, but without any recursive completion
        ## (asserted not to be needed if the subclass slot is complete)
        for(i in seq_along(subclasses)) {
            obji <- subclasses[[i]]
            ## don't override existing relations
            ## TODO:  have a metric that picks the "closest" relationship
            if(!extends(obji@subClass, class2))
                setIs(obji@subClass, class2, extensionObject = obji, doComplete = FALSE,
                      where = where)
        }
    }
## TODO:  move these checks to a tool used by check & conditional on no .S3Class slot
##     S3Class <- attr(ClassDef@prototype, ".S3Class")
##     if(!is.null(S3Class)) {
##       others <- c(ClassDef@className, names(exts))
##       others <- others[is.na(match(others, S3Class))]
##       if(length(others)>0)
##         .checkS3forClass(ClassDef@className, where, others)
##     }
    exts
}

completeSubclasses <-
    function(classDef, class2, extensionDef, where, classDef2 = getClassDef(class2, where)) {
    ## check for indirect extensions => already completed
    ext <- classDef@subclasses
    for(i in seq_along(ext)) {
        if(.isIndirectExtension(ext[[i]])) {
            classDef <- .uncompleteClassDefinition(classDef, "subclasses")
            break
        }
    }
    subclasses <- .walkClassGraph(classDef, "subclasses", where)
    if(!missing(class2) && length(classDef@contains)) {
        strictBy <- TRUE
        contains <-
            .transitiveExtends(class2, classDef@className, extensionDef, classDef@contains, strictBy)
        ## insert the new is relationship, but without any recursive completion
        ## (asserted not to be needed if the subclass slot is complete)
        for(i in seq_along(contains)) {
            obji <- contains[[i]]
            cli <- contains[[i]]@superClass
            cliDef <- getClassDef(cli, where)
            ## don't override existing relations
            ## TODO:  have a metric that picks the "closest" relationship
            if(!extends(classDef2, cliDef))
                setIs(class2, cli, extensionObject = obji,
                      doComplete = FALSE, where = where)
        }
    }
    subclasses
}


## utility function to walk the graph of super- or sub-class relationships
## in order to incorporate indirect relationships
.walkClassGraph <-  function(ClassDef, slotName, where,  conflicts = character())
{
    ext <- slot(ClassDef, slotName)
    if(length(ext) == 0)
        return(ext)
    className <- ClassDef@className
    ## the super- vs sub-class is identified by the slotName
    superClassCase <- identical(slotName, "contains")
    what <- names(ext)
    for(i in seq_along(ext)) { # note that this loops only over the original ext
        by <- what[[i]]
        if(isClass(by, where = where)) {
            byDef <- getClass(by, where = where)
            exti <-  slot(byDef, slotName)
            coni <- attr(exti, "conflicts") # .resolveSuperclasses makes this
            if(superClassCase && length(coni) > 0) {
                conflicts <- unique(c(conflicts, coni))
              }
            ## add in those classes not already known to be super/subclasses
            exti <- exti[is.na(match(names(exti), what))]
            if(length(exti)) {
                if(superClassCase) {
                    strictBy <- TRUE  # FIXME:  need to find some safe test allowing non-strict
                      exti <- .transitiveExtends(className, by, ext[[i]], exti, strictBy)
                }
                else {
                    strictBy <- TRUE
                    exti <- .transitiveSubclasses(by, className, ext[[i]], exti, strictBy)
                }
                ext <- c(ext, exti)
            }
        }
        else
            stop(gettextf("the '%s' list for class %s, includes an undefined class %s",
                          if(superClassCase) "superClass" else "subClass",
                          dQuote(className),
                          dQuote(.className(by))),
                 domain = NA)
    }
    what <- names(ext)  ## the direct and indirect extensions
    if(!all(is.na(match(what, className)))) {
        ok <- is.na(match(what, className))
        ## A class may not contain itself, directly or indirectly
        ## but a non-simple cyclic relation, involving setIs, is allowed
        for(i in seq_along(what)[!ok]) {
            exti <- ext[[i]]
            if(!is(exti, "conditionalExtension")) {
                if(superClassCase) {
                    whatError <-  "contain itself"
                }
                else {
                    whatError <- "have itself as a subclass"
                }
                ## this is not translatable
                stop(sprintf("class %s may not %s: it contains class %s, with a circular relation back to %s",
                             dQuote(className), whatError,
                             dQuote(exti@by),
                             dQuote(className)),
                     domain = NA)
            }
        }
        ext <- ext[ok]
    }
    ## require superclasses to be sorted by distance
    distOrder <- sort.list(sapply(ext, function(x)x@distance))
    ext <- ext[distOrder]
    if(superClassCase && (anyDuplicated(what) || length(conflicts) > 0))
        ext <- .resolveSuperclasses(ClassDef, ext, where, conflicts)
    ext
}

.reportSuperclassConflicts <- function(className, ext, where) {
    what <- names(ext)
    conflicts <- character()
    for(i in seq_along(ext)) {
        by <- what[[i]]
        ## report only the direct superclass from which inconsistencies are inherited
        if(identical(ext[[i]]@distance, 1) && isClass(by, where = where)) {
            byDef <- getClass(by, where = where)
            exti <-  byDef@contains
            coni <- attr(exti, "conflicts") # .resolveSuperclasses makes this
            if( length(coni) > 0) {
                warning(gettextf("class %s is inheriting an inconsistent superclass structure from class %s, inconsistent with %s",
                                 .dQ(className), .dQ(by),
                                 paste(.dQ(coni), collapse = ", ")),
                        call. = FALSE, domain = NA)
                conflicts <- unique(c(conflicts, coni))
              }
          }
      }
          newconflicts <- attr(ext, "conflicts")
        if(length(newconflicts) > length(conflicts))
          warning(gettextf("unable to find a consistent ordering of superclasses for class %s: order chosen is inconsistent with the superclasses of %s",
                           .dQ(className),
                           paste(.dQ(setdiff(newconflicts, conflicts)),
                                 collapse = ", ")),
                  call. = FALSE, domain = NA)
        }


.resolveSuperclasses <- function(classDef, ext, where, conflicts = attr(ext, "conflicts")) {
  ## find conditional extensions, ignored in superclass ordering
  .condExts <- function(contains)
      sapply(contains, function(x) is(x, "conditionalExtension" ))
  .noncondExtsClass <- function(cl) {
    if(isClass(cl, where = where) ) {
      contains <- getClass(cl, where = where)@contains
      names(contains)[!.condExts(contains)]
    }
    else cl
  }
  what <- names(ext)
  dups <- unique(what[duplicated(what)])
  if(length(dups) > 0) {
    ## First, eliminate all conditional relations, which never override non-conditional
    affected <- match(what, dups, 0) > 0
    conditionals <- .condExts(ext)
    if(any(conditionals)) {
      affected[conditionals] <- FALSE
      what2 <- what[affected]
      dups <- unique(what2[duplicated(what2)])
      if(length(dups) == 0) {
        ##  eliminating conditonal relations removed duplicates
        if(length(conflicts) > 0)
          attr(ext, "conflicts") <- unique(c(conflicts, attr(ext, "conflicts")))
        return(ext)
      }
      ## else, go on with conditionals eliminated
    }
    directSupers <- sapply(classDef@contains, function(x) identical(x@distance, 1))
    directSupers <- unique(names(classDef@contains[directSupers]))
    ## form a list of the superclass orderings of the direct superclasses
    ## to check consistency with each way to eliminate duplicates
    ## Once again, conditional relations are eliminated
    superExts <- lapply(directSupers, .noncondExtsClass)
    names(superExts) <- directSupers
    retain = .choosePos(classDef@className, what, superExts, affected)
    if(is.list(retain)) {
      these <- retain[[2]]
      conflicts <- unique(c(conflicts, these)) # append the new conflicts
      retain <- retain[[1]]
    }
    ## eliminate the affected & not retained
    affected[retain] <- FALSE
    ext <- ext[!affected]
  }
  ## even if no dups here, may have inherited some conflicts,
  ## which will be copied to the contains list.
  ## FUTURE NOTE (7/09):  For now, we are using an attribute for conflicts,
  ## rather than promoting the ext list to a new class, which may be desirable
  ## if other code comes to depend on the conflicts information.
  attr(ext, "conflicts") <- conflicts
  ext
}

classMetaName <-
  ## a name for the object storing this class's definition
  function(name)
  methodsPackageMetaName("C", name)

# regexp for matching class metanames; semi-general but assumes the
# meta pattern starts with "." and has no other special characters
.ClassMetaPattern <- function()
    paste0("^[.]",substring(methodsPackageMetaName("C",""),2))

##FIXME:  C code should take multiple strings in name so paste() calls could  be avoided.
methodsPackageMetaName <-
  ## a name mangling device to simulate the meta-data in S4
  function(prefix, name, package = "")
  ## paste(".", prefix, name, sep="__") # too slow
    .Call(C_R_methodsPackageMetaName, prefix, name, package)

## a  non-exported regexp that matches  methods metanames
## This is quite general and matches all patterns that could be generated
## by calling methodsPackageMetaName() with a sequence of capital Latin letters
## Used by package.skeleton in utils
.methodsPackageMetaNamePattern <- "^[.]__[A-Z]+__"

requireMethods <-
  ## Require a subclass to implement methods for the generic functions, for this signature.
  ##
  ## For each generic, `setMethod' will be called to define a method that throws an error,
  ## with the supplied message.
  ##
  ## The `requireMethods' function allows virtual classes to require actual classes that
  ## extend them to implement methods for certain functions, in effect creating an API
  ## for the virtual class.  Otherwise, default methods for the corresponding function would
  ## be called, resulting in less helpful error messages or (worse still) silently incorrect
  ## results.
  function(functions, signature,
           message = "", where = topenv(parent.frame()))
{
    for(f in functions) {
        method <- getMethod(f, optional = TRUE)
        if(!is.function(method))
            method <- getGeneric(f, where = where)
        body(method) <- substitute(stop(methods:::.missingMethod(FF, MESSAGE, if(exists(".Method")).Method else NULL), domain=NA), list(FF=f, MESSAGE=message))
        environment(method) <- .GlobalEnv
        setMethod(f, signature, method, where = where)
    }
    NULL
}

## Construct an error message for an unsatisfied required method.
.missingMethod <- function(f, message = "", method) {
    if(nzchar(message))
        message <- paste0("(", message, ")")
    message <- paste("for function", f, message)
    if(is(method, "MethodDefinition")) {
        target <-  paste(.dQ(method@target), collapse=", ")
        defined <- paste(.dQ(method@defined), collapse=", ")
        message <- paste("Required method", message, "not defined for signature",
                         target)
        if(!identical(target, defined))
            message <- paste(message, ", required for signature", defined)
    }
    else message <- paste("Required method not defined", message)
    message
}

getSlots <- function(x) {
    classDef <- if(isClassDef(x)) x else getClass(x)
    props <- classDef@slots
    value <- as.character(props)
    names(value) <- names(props)
    value
}


## check for reserved slot names.  Currently only "class" is reserved
validSlotNames <- function(names) {
    if(is.na(match("class", names)))
        names
    else
        stop("\"class\" is a reserved slot name and cannot be redefined")
}

### utility function called from primitive code for "@"
getDataPart <- function(object) {
    if(identical(typeof(object),"S4")) {
        ## explicit .Data or .xData slot
        ## Some day, we may merge both of these as .Data
        value <- attr(object, ".Data")
        if(is.null(value)) {
            value <- attr(object, ".xData")
            if(is.null(value))
              stop("Data part is undefined for general S4 object")
          }
        if(identical(value, .pseudoNULL))
          return(NULL)
        else
          return(value)
    }
    temp <- getClass(class(object))@slots
    if(length(temp) == 0L)
        return(object)
    if(is.na(match(".Data", names(temp))))
       stop(gettextf("no '.Data' slot defined for class %s",
                     dQuote(class(object))),
            domain = NA)
    dataPart <- temp[[".Data"]]
    switch(dataPart,
           ## the common cases, for efficiency
           numeric = , vector = , integer = , character = , logical = ,
           complex = , list =
              attributes(object) <- NULL,
           matrix = , array = {
               value <- object
               attributes(value) <- NULL
               attr(value, "dim") <- attr(object, "dim")
               attr(value, "dimnames") <- attr(object, "dimnames")
               object <- value
           },
           ts = {
               value <- object
               attributes(value) <- NULL
               attr(value, "ts") <- attr(object, "ts")
               object <- value
           },
           ## default:
           if(is.na(match(dataPart, .BasicClasses))) {
               ## keep attributes not corresponding to slots
               attrVals <- attributes(object)
               attrs <- names(attrVals)
               attrs <- attrs[is.na(match(attrs, c("class", names(temp))))]
               attributes(object) <- attrVals[attrs]
           }
           else
           ## other basic classes have no attributes
               attributes(object) <- NULL
           )
    object
}

setDataPart <- function(object, value, check = TRUE) {
    if(check || identical(typeof(object), "S4")) {
        classDef <- getClass(class(object))
        slots <- getSlots(classDef)
        dataSlot <- .dataSlot(names(slots))
        if(length(dataSlot) == 1)
          dataClass <- elNamed(slots, dataSlot)
        else if(check)
          stop(gettextf("class %s does not have a data part (a .Data slot) defined",
                        dQuote(class(object))),
               domain = NA)
        else # this case occurs in making the methods package. why?
          return(.mergeAttrs(value, object))
        value <- as(value, dataClass)  # note that this is strict as()
        if(identical(typeof(object), "S4")) {
            if(is.null(value))
              value <- .pseudoNULL
            attr(object, dataSlot) <- value
            return(object)
        }
    }
    .mergeAttrs(value, object)
}

.validDataPartClass <- function(cl, where, prevDataPartClass = NULL) {
    if(is(cl, "classRepresentation")) {
        ClassDef <- cl
        cl <- ClassDef@className
    }
    else
        ClassDef <- getClass(cl, TRUE)

    switch(cl, matrix = , array = value <- cl,
           value <- elNamed(ClassDef@slots, ".Data"))
    if(is.null(value)) {
        if(.identC(cl, "structure"))
            value <- "vector"
        else if((extends(cl, "vector") || !is.na(match(cl, .BasicClasses))))
            value <- cl
        else if(extends(cl, "oldClass") && isVirtualClass(cl)) {
        }
        else if(identical(ClassDef@virtual, TRUE) &&
               length(ClassDef@slots) == 0L &&
               length(ClassDef@subclasses) ) {
                ## look for a union of basic classes
                subclasses <- ClassDef@subclasses
                what <- names(subclasses)
                value <- cl
                for(i in seq_along(what)) {
                    ext <- subclasses[[i]]
                    ##TODO:  the following heuristic test for an "original"
                    ## subclass should be replaced by a suitable class (extending SClassExtension)
                    if(length(ext@by) == 0L && ext@simple && !ext@dataPart &&
                       is.na(match(what[i], .BasicClasses))) {
                        value <- NULL
                        break
                    }
                }
            }
    }
    if(!(is.null(value) || is.null(prevDataPartClass) || extends(prevDataPartClass, value) ||
         isVirtualClass(value, where = where))) {
      warning(gettextf("more than one possible class for the data part: using %s rather than %s",
                  .dQ(prevDataPartClass), .dQ(value)), domain = NA)
      value <- NULL
    }
    value
}

.dataSlot <- function(slotNames) {
    dataSlot <- c(".Data", ".xData")
    dataSlot <- dataSlot[match(dataSlot, slotNames, 0)>0]
    if(length(dataSlot) > 1)
      stop("class cannot have both an ordinary and hidden data type")
    dataSlot
  }


.mergeAttrs <- function(value, object, explicit = NULL) {
    supplied <- attributes(object)
    if(length(explicit))
        supplied[names(explicit)] <- explicit
    valueAttrs <- attributes(value)
    ## names are special.
    if(length(supplied$names) && length(valueAttrs$names) == 0L) {
        if(length(value) != length(object))
            length(supplied$names) <- length(value)
    }
    if(length(valueAttrs)) {	 ## don't overwrite existing attrs
	valueAttrs$class <- NULL ## copy in class if it's supplied
	supplied[names(valueAttrs)] <- valueAttrs
    } ## else --  nothing to protect
    attributes(value) <- supplied
    if(isS4(object))
        .asS4(value)
    else
        value
}

.newExternalptr <- function()
    .Call(C_R_externalptr_prototype_object)

## modify the list moreExts, currently from class `by', to represent
## extensions instead from an originating class; byExt is the extension
## from that class to `by'
.transitiveExtends <- function(from, by, byExt, moreExts, strictBy) {
    what <- names(moreExts)
###    if(!strictBy) message("Extends: ",from, ": ", paste(what, collapse = ", "))
    for(i in seq_along(moreExts)) {
        toExt <- moreExts[[i]]
        to <- what[[i]]
        toExt <- .combineExtends(byExt, toExt, by, to, strictBy)
        moreExts[[i]] <- toExt
    }
    moreExts
###    if(!strictBy) message("Done")
}

.transitiveSubclasses <- function(by, to, toExt, moreExts, strictBy) {
    what <- names(moreExts)
###    if(!strictBy) message("Subclasses: ",by, ": ", paste(what, collapse = ", "))
    for(i in seq_along(moreExts)) {
        byExt <- moreExts[[i]]
        byExt <- .combineExtends(byExt, toExt, by, to, strictBy)
        moreExts[[i]] <- byExt
    }
    moreExts
###    if(!strictBy) message("Done")
}

.combineExtends <- function(byExt, toExt, by, to, strictBy) {
        ## construct the composite coerce method, taking into account the strict=
        ## argument.
        f <- toExt@coerce
        fR <- toExt@replace
            toExpr <- body(f)
            fBy <- byExt@coerce
            byExpr <- body(fBy)
        ## if both are simple extensions, so is the composition
        if(byExt@simple && toExt@simple) {
            expr <- (if(byExt@dataPart)
                     substitute({if(strict) from <- from@.Data; EXPR},
                                list(EXPR = toExpr))
                   else if(toExt@dataPart)
                     substitute({from <- EXPR;  if(strict) from@.Data},
                                list(EXPR = byExpr))
                   else  (if(identical(byExpr, quote(from)) && identical(toExpr, quote(from)))
                           quote(from)
                         else
                           substitute({from <- E1; E2}, list(E1 = byExpr, E2 = toExpr))
                         )
                     )
            body(f, envir = environment(f)) <- expr
        }
        else {
            toExt@simple <- FALSE
            if(!identical(byExpr, quote(from)))
                body(f, envir = environment(f)) <-
                    substitute( {from <- as(from, BY, strict = strict); TO},
                               list(BY = by, TO = toExpr))
        }
        toExt@coerce <- f
        f <- toExt@test
        toExpr <- body(f)
        byExpr <- body(byExt@test)
        ## process the test code
        if(!identical(byExpr, TRUE)) {
            if(!identical(toExpr, TRUE))
                body(f, envir = environment(f)) <- substitute((BY) && (TO),
                              list(BY = byExpr, TO = toExpr))
            else
                body(f, envir = environment(f)) <- byExpr
        }
        toExt@test <- f
        f <- byExt@replace
        byExpr <- body(f)
        if(!strictBy) {
            toDef <- getClassDef(to)
            byDef <- getClassDef(by)
            strictBy <- is.null(toDef) || is.null(byDef) || toDef@virtual || byDef@virtual
        }
        ## Is there a danger of infinite loop below?
        expr <- substitute({.value <- as(from, BY, STRICT); as(.value, TO) <- value; value <- .value; BYEXPR},
                           list(BY=by, TO = to, BYEXPR = byExpr, STRICT = strictBy))
        body(f, envir = environment(f)) <- expr
        toExt@replace <- f
        toExt@by <- toExt@subClass
        toExt@subClass <- byExt@subClass
        toExt@distance <- toExt@distance + byExt@distance
        ## the combined extension is conditional if either to or by is conditional
        if(is(byExt, "conditionalExtension") && !is(toExt, "conditionalExtension"))
          class(toExt) <- class(byExt)
        toExt
}

## construct the expression that implements the computations for coercing
## an object to one of its superclasses
## The fromSlots argument is provided for calls from makeClassRepresentation
## and completeClassDefinition,
## when the fromClass is in the process of being defined, so slotNames() would fail
.simpleCoerceExpr <- function(fromClass, toClass, fromSlots, toDef) {
    toSlots <- names(toDef@slots)
    sameSlots <- (length(fromSlots) == length(toSlots) &&
		  !any(is.na(match(fromSlots, toSlots))))
    if(!isVirtualClass(toDef))
        toClass <- class(new(toDef)) # get it with the package slot correct
    if(sameSlots)
	substitute({class(from) <- CLASS; from}, list(CLASS = toClass))
    else if(length(toSlots) == 0L) {
	## either a basic class or something with the same representation
	if(is.na(match(toClass, .BasicClasses)))
	    substitute({ attributes(from) <- NULL; class(from) <- CLASS; from},
		       list(CLASS = toClass))
	else if(isVirtualClass(toDef))
	    quote(from)
	else {
	    ## a basic class; a vector type, matrix, array, or ts
	    switch(toClass,
		   matrix = , array = {
		       quote({.dm <- dim(from); .dn <- dimnames(from)
			      attributes(from) <- NULL; dim(from) <- .dm
			      dimnames(from) <- .dn; from})
		   },
		   ts = {
		       quote({.tsp <- tsp(from); attributes(from) <- NULL
			      tsp(from) <- .tsp; class(from) <- "ts"; from})
		   },
		   quote({attributes(from) <- NULL; from})
		   )
	}
    }
    else {
	substitute({ value <- new(CLASS)
		     for(what in TOSLOTS)
			 slot(value, what) <- slot(from, what)
		     value },
		   list(CLASS = toClass, TOSLOTS = toSlots))
    }
}

.simpleReplaceExpr <- function(toDef) {
    toSlots <- names(toDef@slots)
    substitute({
        for(what in TOSLOTS)
            slot(from, what) <- slot(value, what)
        from
    }, list(TOSLOTS = toSlots))
}

## the boot version of newClassRepresentation (does no checking on slots to avoid
## requiring method selection on coerce).

newClassRepresentation <- function(...) {
    value <- new("classRepresentation")
    slots <- list(...)
    slotNames <- names(slots)
    for(i in seq_along(slotNames))
        slot(value, slotNames[[i]], FALSE) <- slots[[i]]
    value
}

## create a temporary definition of a class, but one that is distinguishable
## (by its class) from the real thing.  See comleteClassDefinition
.tempClassDef <- function(...) {
    value <- new("classRepresentation")
    slots <- list(...)
    slotNames <- names(slots)
    for(i in seq_along(slotNames))
        slot(value, slotNames[[i]], FALSE) <- slots[[i]]
    value
}

## the real version of newClassRepresentation, assigned in ..First.lib
.newClassRepresentation <- function(...)
    new("classRepresentation", ...)

.insertExpr <- function(expr, el) {
    if(!is(expr, "{"))
        expr <- substitute({EXPR}, list(EXPR = expr))
    expr[3L:(length(expr)+1)] <- expr[2L:length(expr)]
    expr[[2L]] <- el
    expr
}

## utility guaranteed to return only the first string of the class.
## Would not be needed if we dis-allowed S3 classes with multiple strings (or
## if the methods package version of class dropped the extra strings).
.class1 <- function(x) {
    cl <- class(x)
    if(length(cl) > 1L)
        cl[[1L]]
    else
        cl
}

substituteFunctionArgs <-
    function(def, newArgs, args = formalArgs(def), silent = FALSE,
             functionName = "a function")
{
    if(!identical(args, newArgs)) {
        if( !missing(functionName) ) # this style does not allow translation
            functionName <- paste("for", functionName)

        n <- length(args)
        if(n != length(newArgs))
            stop(sprintf("trying to change the argument list of %s with %d arguments to have arguments (%s)",
                         functionName, n, paste(newArgs, collapse = ", ")),
                 domain = NA)
        bdy <- body(def)
        ## check for other uses of newArgs
        checkFor <- newArgs[is.na(match(newArgs, args))]
        locals <- all.vars(bdy)
        if(length(checkFor) && any(!is.na(match(checkFor, locals))))
            stop(sprintf("get rid of variables in definition %s (%s); they conflict with the needed change to argument names (%s)",
                         functionName,
                         paste(checkFor[!is.na(match(checkFor, locals))], collapse = ", "),
                         paste(newArgs, collapse = ", ")), domain = NA)
        ll <- vector("list", 2L*n)
        for(i in seq_len(n)) {
            ll[[i]] <- as.name(args[[i]])
            ll[[n+i]] <- as.name(newArgs[[i]])
        }
        names(ll) <- c(args, newArgs)
        body(def, envir = environment(def)) <- substituteDirect(bdy, ll)
        if(!silent) {
            msg <-
                sprintf("NOTE: arguments in definition %s changed from (%s) to (%s)",
                        functionName,
                        paste(args, collapse = ", "),
                        paste(newArgs, collapse = ", "))
            message(msg, domain = NA)
        }
    }
    def
}

.makeValidityMethod <- function(Class, validity) {
    if(!is.null(validity)) {
        if(!is(validity, "function"))
            stop(gettextf("a validity method must be a function of one argument, got an object of class %s",
                          dQuote(class(validity))),
                 domain = NA)
        validity <- substituteFunctionArgs(validity, "object", functionName = sprintf("validity method for class '%s'", Class))
    }
    validity
}

# the bootstrap version of setting slots in completeClassDefinition
.mergeClassDefSlots <- function(ClassDef, ...) {
    slots <- list(...); slotNames <- names(slots)
    for(i in seq_along(slots))
        slot(ClassDef, slotNames[[i]], FALSE) <- slots[[i]]
    ClassDef
}

## the real version:  differs only in checking the slot values
..mergeClassDefSlots <- function(ClassDef, ...) {
    slots <- list(...); slotNames <- names(slots)
    for(i in seq_along(slots))
        slot(ClassDef, slotNames[[i]]) <- slots[[i]]
    ClassDef
}

### fix the annoying habit of R giving function definitions the local environment by default
.gblEnv <- function(f) {
    environment(f) <- .GlobalEnv
    f
}

## a utility for makePrototypeFromClassDef that causes inf. recursion if used too early
..isPrototype <- function(p)is(p, "classPrototypeDef")
## the simple version
.isPrototype <- function(p) .identC(class(p), "classPrototypeDef")

.className <- function(cl) if(is(cl, "classRepresentation")) cl@className else as(cl, "character")

## bootstrap version:  all classes and methods must be in the version of the methods
## package being built in the toplevel environment: MUST avoid require("methods") !
.requirePackage <- function(package, mustFind = TRUE)
    topenv(parent.frame())

.PackageEnvironments <- new.env(hash=TRUE) # caching for required packages

## real version of .requirePackage
..requirePackage <- function(package, mustFind = TRUE) {
    value <- package
    if(nzchar(package)) {
        if(package %in% loadedNamespaces())
            value <- getNamespace(package)
        else {
            if(identical(package, ".GlobalEnv"))
                return(.GlobalEnv)
            if(identical(package, "methods"))
                return(topenv(parent.frame())) # booting methods
            if(exists(package, envir = .PackageEnvironments, inherits = FALSE))
                return(get(package, envir = .PackageEnvironments)) #cached, but only if no namespace
        }
    }
    if(is.environment(value))
        return(value)
    topEnv <- options()$topLevelEnvironment
    if(is.null(topEnv))
        topEnv <- .GlobalEnv
    if(exists(".packageName", topEnv, inherits=TRUE) &&
       .identC(package, get(".packageName", topEnv)))
        return(topEnv) # kludge for source'ing package code
    if(nzchar(package) && require(package, character.only = TRUE)) {}
    else {
        if(mustFind)
          stop(gettextf("unable to find required package %s",
                        sQuote(package)),
               domain = NA)
        else
          return(NULL)
    }
    value <- .asEnvironmentPackage(package)
    assign(package, value, envir = .PackageEnvironments)
    value
}

.classDefEnv <- function(classDef) {
    .requirePackage(classDef@package)
}


.asEnvironmentPackage <- function(package) {
    if(identical(package, ".GlobalEnv"))
        .GlobalEnv
    else {
        ##FIXME:  the paste should not be needed
        pkg <- paste("package", package, sep=":")
        ## need to allow for versioned installs: prefer exact match.
        m <- charmatch(pkg, search())
        if(is.na(m)) # not attached, better be an available namespace
            getNamespace(package)
        else
          as.environment(search()[m])
    }
}

## bootstrap version, mustn't fail
.classEnv <- function(Class, default = .requirePackage("methods"), mustFind = TRUE) {
         package <- packageSlot(Class)
        if(is.null(package)) {
            ## unconditionally use the methods package
            default
        }
        else
            .requirePackage(package)
     }


## to be .classEnv()  --- currently used in 'Matrix'  (via wrapper)
..classEnv <- function(Class, default = .requirePackage("methods"), mustFind = TRUE) {
    package <- { if(is.character(Class)) packageSlot(Class) else
		 ## must then be a class definition
		 Class@package }
    if(is.null(package)) {
	## use the default, but check that the class is there, and if not
	## try a couple of other heuristics
	value <- default
	def <- getClassDef(Class, value, NULL)
	if(is.null(def)) {
	    value <- .GlobalEnv
	    def <- getClassDef(Class, value, NULL)
	    if(is.null(def)) {
		value <- .requirePackage("methods")
		if(!identical(default, value)) # user supplied default
		    def <- getClassDef(Class, value, NULL)
	    }
	}
	if(is.null(def) && mustFind)
	    stop(gettextf("unable to find an environment containing class %s",
			  dQuote(Class)),
                 domain = NA)
	value
    }
    else
	.requirePackage(package)
}

## find a generic function reference, using the package slot if present
## FIXME:  this and .classEnv should be combined and implemented in C for speed
## They differ in that  .classEnv uses the class metaname when it searches; i.e.,
## they use getClassDef and .getGeneric resp.  Also, .getEnv returns baseenv() rather
## than generating an error if no generic found (so getGeneric can return gen'c for prim'ves)

.genEnv <-  function(f, default = .requirePackage("methods"), package = "")
{
    if(!nzchar(package))
        package <- packageSlot(f)
    if(is.null(package)) {
        ## use the default, but check that the object is there, and if not
        ## try a couple of other heuristics
        value <- default
        def <- .getGeneric(f, value)
        if(is.null(def)) {
            value <- .GlobalEnv
            def <- .getGeneric(f, value)
            if(is.null(def)) {
                value <- .requirePackage("methods")
                if(!identical(default, value)) # user supplied default
                    def <- .getGeneric(f, value)
            }
        }
        if(is.null(def))
            baseenv()
        else
            value
    }
    else
        .requirePackage(package)
}

## cache and retrieve class definitions  If there is a conflict with
## packages a list of  classes will be cached
## See .cacheGeneric, etc. for analogous computations for generics
.classTable <- new.env(TRUE, baseenv())
assign("#HAS_DUPLICATE_CLASS_NAMES", FALSE, envir = .classTable)
.duplicateClassesExist <- function(on) {
    value <- get("#HAS_DUPLICATE_CLASS_NAMES", envir = .classTable)
    if(nargs())
        assign("#HAS_DUPLICATE_CLASS_NAMES", on, envir = .classTable)
    value
}

.cacheClass <- function(name, def, doSubclasses = FALSE, env) {
    if(!identical(doSubclasses, FALSE))
      .recacheSubclasses(def@className, def, doSubclasses, env)
    if(exists(name, envir = .classTable, inherits = FALSE)) {
        newpkg <- def@package
        prev <- get(name, envir = .classTable)
        if(is(prev, "classRepresentation")) {
            if(identical(prev, def))
               return()
            pkg <- prev@package # start a per-package list
            if(identical(pkg, newpkg)) { # redefinition
                ## cache for S3, to override possible previous cache
                base:::.cache_class(name, .extendsForS3(def))
##                base:::.cache_class(name, extends(def))
                return(assign(name, def, envir = .classTable))
            }
            else if(.simpleDuplicateClass(def, prev))
                return()
            prev <- list(prev)
            names(prev) <- pkg
        }
        i <- match(newpkg, names(prev))
        if(is.na(i))
           prev[[newpkg]] <- def
        else if(identical(def, prev[[i]]))
          return()
        else
            prev[[i]] <- def
        def <- prev
        .duplicateClassesExist(TRUE)
    }
    assign(name, def, envir = .classTable)
}

## test for identical def, prev class definitions
## An exhaustive test would be very complicated, having to test
## superclasses in detail, prototypes for the slots, etc.
.simpleDuplicateClass <- function(def, prev) {
    supers <- names(def@contains)
    prevSupers <- names(prev@contains)
    if(length(supers) != length(prevSupers) ||
       any(is.na(match(supers, prevSupers))))
        return(FALSE)
    warnLevel <- getOption("warn")
    S3 <- "oldClass" %in% supers
    if(S3) {
        ## it is possible one  of these is inconsistent, but unlikely
        ## and we will get here often from multiple setOldClass(...)'s
        if(warnLevel)
            message(gettextf("Note: the specification for S3 class %s in package %s seems equivalent to one from package %s: not turning on duplicate class definitions for this class.",
                             dQuote(def@className),
                             sQuote(def@package),
                             sQuote(prev@package)),
                    domain = NA)
        return(TRUE)
    }
    ## if there are already duplicate classes, we check duplicates
    ## for the superclasses
    dupsExist <- .duplicateClassesExist()
    if(dupsExist) {
        dups <- match(supers, multipleClasses(), 0) > 0
        if(any(dups)) {
            if(warnLevel)
                message(gettextf("Note: some superclasses of class %s in package %s have duplicate definitions.  This definition is not being treated as equivalent to that from package %s",
                                 dQuote(def@className),
                                 sQuote(def@package),
                                 sQuote(prev@package)),
                    domain = NA)
            return(FALSE)
        }
    }
    ## now check the slots
    slots <- names(def@slots)
    prevSlots <- names(prev@slots)
    if(length(slots) != length(prevSlots) ||
       any(is.na(match(slots, prevSlots))))
        return(FALSE)
    for(what in slots) {
        slotClasses <- def@slots
        prevClasses <- prev@slots
        clWhat <- slotClasses[[what]]
        prevWhat <- prevClasses[[what]]
        if(!identical(as.character(clWhat), as.character(prevWhat)) ||
           (dupsExist && !identical(as.character(packageSlot(clWhat)),
              as.character(packageSlot(prevWhat)))))
            return(FALSE)
    }
    if(warnLevel)
        message(gettextf("Note: the specification for class %s in package %s seems equivalent to one from package %s: not turning on duplicate class definitions for this class.",
                         dQuote(def@className),
                         sQuote(def@package),
                         sQuote(prev@package)),
                    domain = NA)
    TRUE
}

.uncacheClass <- function(name, def) {
    if(exists(name, envir = .classTable, inherits = FALSE)) {
        if(is(def, "classRepresentation")) # paranoia: should only be called this way
            newpkg <- def@package
        else
            newpkg <- ""
        prev <- get(name, envir = .classTable)
        if(is(prev, "classRepresentation") &&
           identical(prev@package, newpkg) )
            return(remove(list = name, envir = .classTable))
         i <- match(newpkg, names(prev))
        if(!is.na(i))
           prev[[i]] <- NULL
        else # we might warn about unchaching more than once
          return()
        if(length(prev) == 0L)
          return(remove(list = name, envir = .classTable))
        else if(length(prev) == 1L)
          prev <- prev[[1L]]
        assign(name, prev, envir  = .classTable)
    }
}

## the workhorse of class access
## The underlying C code will return name if it is not a character vector
## in the assumption this is a classRepresentation or subclass of that.
## In principle, this could replace the checks on class(name) in getClassDef
## and new(), which don't work for subclasses of classRepresentation anyway.
.getClassFromCache <- function(name, where) {
	value <- .Call(C_R_getClassFromCache, name, .classTable)
	if(is.list(value)) { ## multiple classes with this name
	    pkg <- packageSlot(name)
	    if(is.null(pkg))
		pkg <- if(is.character(where)) where else getPackageName(where, FALSE) # may be ""
	    pkgs <- names(value)
	    i <- match(pkg, pkgs, 0L)
	    if(i == 0L) ## try 'methods':
		i <- match("methods", pkgs, 0L)
	    if(i > 0L) value[[i]]
            else NULL
	}
	else #either a class definition or NULL
	    value
}

### insert superclass information into all the subclasses of this
### class.  Used to incorporate inheritance information from
### ClassUnions
.recacheSubclasses <- function(class, def, doSubclasses, env) {
    subs <- def@subclasses
    subNames <- names(subs)
    for(i in seq_along(subs)) {
        what <- subNames[[i]]
        subDef <- getClassDef(what, env)
        if(is.null(subDef))
            warning(gettextf("undefined subclass %s of class %s; definition not updated",
                             .dQ(what), .dQ(def@className)))
        else if(is.na(match(what, names(subDef@contains)))) {
            ## insert the new superclass to maintain order by distance
            cntns <- subDef@contains
            cntns[[class]] <- subs[[i]]
            cntns <- cntns[sort.list(sapply(cntns, function(x)x@distance))]
            subDef@contains <- cntns
            .cacheClass(what, subDef, FALSE, env)
        }
    }
    NULL
}

## alternative to .recacheSubclasses, only needed for non-unions
## Inferior in that nonlocal subclasses will not be updated, hence the
## warning when the subclass is not in where
.checkSubclasses <- function(class, def, class2, def2, where, where2) {
    where <- as.environment(where)
    where2 <- as.environment(where2)
   subs <- def@subclasses
    subNames <- names(subs)
    extDefs <- def2@subclasses
    for(i in seq_along(subs)) {
        what <- subNames[[i]]
        if(.identC(what, class2))
          next # catch recursive relations
        cname <- classMetaName(what)
        if(exists(cname, envir = where, inherits = FALSE)) {
            subDef <- get(cname, envir = where)
            cwhere <- where
        }
        else if(exists(cname, envir = where2, inherits = FALSE)) {
            subDef <- get(cname, envir = where2)
            cwhere <- where2
        }
        else {
          warning(gettextf("subclass %s of class %s is not local and cannot be updated for new inheritance information; consider setClassUnion()",
                           .dQ(what), .dQ(class)),
                  call. = FALSE, domain = NA)
          next
        }
        extension <- extDefs[[what]]
        if(is.null(extension)) # not possible if the setIs behaved?
          warning(gettextf("no definition of inheritance from %s to %s, though the relation was implied by the setIs() from %s",
                           .dQ(what), .dQ(def2@className), .dQ(class)),
                  call. = FALSE, domain = NA)
        else if(is.na(match(class2, names(subDef@contains)))) {
            subDef@contains[[class2]] <- extension
            assignClassDef(what, subDef, cwhere, TRUE)
        }
    }
    NULL
}

.removeSuperclassBackRefs <- function(Class, classDef, classWhere)
{
    if(length(classDef@contains)) {
        superclasses <- names(classDef@contains)
        for(what in superclasses) {
            cdef <- .getClassFromCache(what)
            ## TODO:  handle the case of multiple packages with this class
            if(is(cdef, "classRepresentation"))
                .removeSubClass(what, Class, cdef)
        }
    }
    NULL
}


## remove subclass from the list of subclasses of class
## in the cache and possibly in the attached package environment
.removeSubClass <- function(class, subclass, cdef) {
    if(is.null(cdef)) {}
    else {
        newdef <- .deleteSubClass(cdef, subclass)
        if(!is.null(newdef))
            .cacheClass(class, newdef, FALSE, cdef@package)
        ## the class definition in the search list may have been altered
        ## (e.g., when classes are created in the global environment_
        pname <- cdef@package
        if(identical(pname, ".GlobalEnv")) {
            pos <- 1
        }
        else {
            pname <- paste0("package:", pname)
            pos <- match(pname, search(), 0)
        }
        if(pos) {
            penv <- as.environment(pname)
            cmeta <- classMetaName(class)
            if(exists(cmeta, envir = penv, inherits = FALSE)) {
                cdefp <- get(cmeta, envir = penv)
                if(subclass %in% names(cdefp@subclasses)) {
                    newdef <- .deleteSubClass(cdefp, subclass)
                    if(!is.null(newdef)) {
                        ## unfortunately, assignClassDef assigns the subclass info
                        ## even in a locked binding.  Would be nice to change that,
                        ## but probably too much would break.
                        if(bindingIsLocked(cmeta, penv))
                            .assignOverBinding(cmeta, newdef, penv, FALSE)
                        else
                            assign(cmeta, newdef, envir = penv)
                    }
                }
            }
        }
    }
    sig <- signature(from=subclass, to=class)
    if(existsMethod("coerce", sig))
        .removeCachedMethod("coerce", sig)
    if(existsMethod("coerce<-", sig))
        .removeCachedMethod("coerce<-", sig)
}

.deleteSubClass <- function(cdef, subclass) {
        subclasses <- cdef@subclasses
        ii <- match(subclass, names(subclasses), 0)
        ## the subclass may not be there, e.g., if that class has been
        ## unloaded.
        if(ii > 0) {
            cdef@subclasses <- subclasses[-ii]
            cdef
        }
        else
          NULL
    }

## remove superclass from  definition of class in the cache & in environments
## on search list
.removeSuperClass <- function(class, superclass) {
    cdef <- .getClassFromCache(class, where)
    if(is.null(cdef)) {}
    else {
        newdef <- .deleteSuperClass(cdef, superclass)
        if(!is.null(newdef))
          .cacheClass(class, newdef, FALSE, where)
    }
    sig <- signature(from=class, to=superclass)
    if(existsMethod("coerce", sig))
      .removeCachedMethod("coerce", sig)
    if(existsMethod("coerce<-", sig))
      .removeCachedMethod("coerce<-", sig)
    evv <- findClass(class, .GlobalEnv) # what about hidden classes?  how to find them?
    mname <- classMetaName(class)
    for(where in evv) {
        if(exists(mname, envir = where, inherits = FALSE)) {
            cdef <- get(mname, envir = where)
            newdef <- .deleteSuperClass(cdef, superclass)
            if(!is.null(newdef)) {
              assignClassDef(class, newdef,  where, TRUE)
              ## message("deleted ",superclass, " from ",class, "in environment")
          }
        }
    }
    NULL
}

.deleteSuperClass <- function(cdef, superclass) {
        superclasses <- cdef@contains
        ii <- match(superclass, names(superclasses), 0)
        if(ii > 0) {
            cdef@contains <- superclasses[-ii]
            for(subclass in names(cdef@subclasses))
              .removeSuperClass(subclass, superclass)
            cdef
        }
        else
          NULL
    }

classesToAM <- function(classes, includeSubclasses = FALSE,
                        abbreviate = 2) {
  .mergeMatrices <- function(m1, m2) {
    if(nrow(m1) == 0)
      return(m2)
    dn1 <- dimnames(m1)
    dn2 <- dimnames(m2)
    rows <- unique(c(dn1[[1]], dn2[[1]]))
    columns <- unique(c(dn1[[2]], dn2[[2]]))
    value <- matrix(0, length(rows), length(columns), dimnames = list(rows, columns))
    value[dn1[[1]], dn1[[2]] ] <- m1
    value[dn2[[1]], dn2[[2]] ] <- m2
    value
  }
  if(length(includeSubclasses) == 1)
    includeSubclasses <- rep.int(includeSubclasses, length(classes))
  if(!is(includeSubclasses, "logical") || length(includeSubclasses) != length(classes))
    stop("argument 'includeSubclasses' must be a logical, either one value or a vector of the same length as argument 'classes'")
  value <- matrix(0,0,0)
  for(i in seq_along(classes)) {
    class <- classes[[i]] # to allow for package attribute
    classDef <- getClass(class) # throws an error if undefined.  Make a warning?
    value <- .mergeMatrices(value, .oneClassToAM(classDef, includeSubclasses[[i]]))
  }
  abbr <- match(as.integer(abbreviate), 0:3)-1
  if(length(abbr) != 1 || is.na(abbr))
    stop("argument 'abbreviate' must be 0, 1, 2, or 3")
  if(abbr %% 2)
    dimnames(value)[[1]] <- base::abbreviate(dimnames(value)[[1]])
  if(abbr %/% 2)
    dimnames(value)[[2]] <- base::abbreviate(dimnames(value)[[2]])
  value
}

.oneClassToAM <- function(classDef, includeSubclasses = FALSE, short = FALSE) {
    findEdges <- function(extensions) {
        superclasses <- names(extensions)
        edges <- numeric()
        for(what in superclasses) {
            whatDef <- getClassDef(what)
            ifrom <- match(what, nodes)
            if(is.null(whatDef) || is.na(ifrom))
              next
            exts <- whatDef@contains
            whatedges <- names(exts)
            ito <- match(whatedges, nodes, 0)
            for(i in seq_along(exts))
              if(ito[[i]] >0 && exts[[i]]@distance == 1)
                edges <- c(edges, ifrom, ito[[i]])
        }
        edges
    }
    nodes <- c(classDef@className, names(classDef@contains))
    if(includeSubclasses)
      nodes <- c(nodes, names(classDef@subclasses))
    nodes <- unique(nodes)
    labels <-
        if(isTRUE(short)) abbreviate(nodes)
        else if(is.character(short)) {
            if(length(short) != length(nodes))
                stop(gettextf("needed the supplied labels vector of length %d, got %d",
                              length(nodes), length(short)), domain = NA)
            else short
        } else nodes
    size <- length(nodes)
    value <- matrix(0, size, size, dimnames = list(labels, labels))
    ifrom <- match(classDef@className, nodes) # well, 1, but just for consistency
    ## the following could use the current fact that direct superclasses come
    ## first, but the efficiency gain is minor, so we use the findEdges logic
    extensions <- classDef@contains
    superclasses <- names(extensions)
    ito <- match(superclasses, nodes)
    edges <- numeric()
    for(i in seq_along(extensions)) {
        exti <- extensions[[i]]
        if(exti@distance == 1)
            edges <- c(edges, ifrom, ito[[i]])
    }
    edges <- c(edges, findEdges(classDef@contains))
    if(includeSubclasses) {
        edges <- c(edges, findEdges(classDef@subclasses))
    }
    edges <- t(matrix(edges, nrow=2))
    value[edges] <- 1
    value
}

.choosePos <- function (thisClass, superclasses, subNames, affected)
  ## find if possible a set of superclass relations that gives a consistent
  ## ordering and eliminates any duplicates in the affected relations
  ## Note that the returned indices are against the index of superclasses
  ## If no successful selection is possible, return (one of) the best
  ## attempt, and the superclass(es) inconsistently embedded
{
    candidates <- list()
    allNames <- c(thisClass, superclasses)
    dups <- unique(superclasses[affected])
    whichCase <- names(subNames)
    for(what in dups) {
        where <- seq_along(allNames)[match( allNames, what,0)>0]
        ## make a list of all the subsets to remove duplicates
        whatRemove <- lapply(-seq_along(where), function(x,y) y[x], y=where)
        if(length(candidates) == 0)
          candidates <- whatRemove
        else # all the pairwise combinations with the previous
          candidates <- outer(candidates, whatRemove,
                              function(x,y)mapply(c,x,y, SIMPLIFY=FALSE))
    }
    ## check each way to make the list unique against each superclass extension
    problems <- function(x,y) any(diff(match(y, x))<0)
    possibles <- lapply(candidates, function(x, names)names[-x], names=allNames)
    ## the next could be vectorized, but here we choose instead to exit early.
    scores <- vector("list", length(possibles))
    for(i in seq_along(possibles)) {
        score <- sapply(subNames, problems, x=possibles[[i]])
        scores[[i]] <- whichCase[score]
        if(!any(score))
          return(-candidates[[i]]+1)
    }
    # the first min. scoring possibility and its score
    i <- which.min(sapply(scores, length))
    list(-candidates[[i]]+1, scores[[i]])
}

.checkGeneric <- function(what, where) {
  .checkFun <-  function(x) {
      maybe <- (if(exists(x, where)) {
        f <- get(x, where)
        is.function(f)
      }
      else
        FALSE)
      if(maybe)
        maybe <- is(f, "genericFunction") ||
              (length(grep("UseMethod", deparse(f))) > 0) ||
              is.primitive(f)
      maybe
    }
  sapply(what, .checkFun)
}


S3forS4Methods <- function(where, checkClasses = character()) {
  allClasses <- getClasses(where)
  if(length(checkClasses) > 0)
    allClasses <- allClasses[match(allClasses, checkClasses, 0) > 0]
  if(length(allClasses) == 0)
    return(allClasses)
  pattern <- paste0("([.]",allClasses, "$)", collapse="|")
  allObjects <- objects(where, all.names = TRUE)
  allObjects <- allObjects[-grep("^[.][_][_]", allObjects)] # remove meta data
  allObjects <- grep(pattern, allObjects, value = TRUE)
  if(length(allObjects) > 0) {
    badMethods <- allObjects
    funs <- sub(pattern, "", badMethods)
    uniqueFuns <- unique(funs)
    uniqueFuns <- uniqueFuns[nzchar(uniqueFuns)]
    possible <- .checkGeneric(uniqueFuns, where)
    if(!any(possible))
      return(character())
    uniqueFuns <- uniqueFuns[possible]
    badMethods <- badMethods[match(funs, uniqueFuns, 0) > 0]
    allObjects <- badMethods
    attr(allObjects, "functions") <- uniqueFuns
  }
  allObjects
}

## ## this function warns of S3 methods for S4 classes, but only once per package
## ## per session.
## .checkS3forS4 <- function(method) {
##   envir <- environment(method)
##   pkg <- getPackageName(envir)
##   if(!nzchar(pkg)) pkg <- getPackageName(parent.env(pkg)) #? if generic function
##   if(!nzchar(pkg)) pkg <- format(envir)
##   if(!exists(".WarnedS3forS4", .GlobalEnv, inherits = FALSE))
##     assign(".WarnedS3forS4", character(), envir = .GlobalEnv)
##   if(is.na(match(pkg, .WarnedS3forS4))) {
##       methods <-   S3forS4Methods(envir)
##       .WarnedS3forS4 <<- c(.WarnedS3forS4, pkg)
##       if(length(methods) > 0) {
##         warning("S3 methods written for S4 classes will fail inheritance!\nPackage ", pkg, " apparently has ",
##             length(methods), " such methods  for the functions ", paste(attr(methods, "functions"), collapse = ", "), "\n\n",
##         "Possible dangerous methods: ", paste(methods, collapse =", "),
##                 "\n\n(Warnings generated once per package per session)")
##       }
##   }
## }

## a warning when a class is defined that extends classes with S3 methods.
## .checkS3forClass <- function(className, where, what = className) {
##   badMethods <- S3forS4Methods(where, what)
##   if(length(badMethods) > 0) {
##     msg <- paste0("The apparent methods are ", paste('"',badMethods, '"', collapse = ", "))
##     warning("Some of the superclasses in the definition of class \"",
##             className, "\" have apparent S3 methods.\n\nThese will be hidden by the S3 class that this class contains. (See ?Methods)\n\n", msg)
##   }
## }

## a utility to detect mixin classes:  meant to be fast for use in
## initialize methods (cf the "matrix" method in BasicClasses.R)
isMixin <- function(classDef) {
    val <- 0
    cc <- classDef@contains
    ## relies on the superclasses in contains slot being ordered by distance
    for(cl in cc) {
        if(cl@distance > 1 || val > 1)
          break
        val <- val + 1
    }
    val > 1
}

.classDefIsLocked <- function(classDef) {
    what <- classMetaName(classDef@className)
    env <- .NamespaceOrEnvironment(classDef@package)
    is.environment(env) && exists(what, envir = env, inherits = FALSE) &&
       bindingIsLocked(what, env)
}

