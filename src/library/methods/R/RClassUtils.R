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
    (length(properties)==0 && is.null(prototype))
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
    dataPartDone <- length(slots)==0 || !is.na(match(".Data", snames))
    dataPartClass <- if(dataPartDone) "ANY" else elNamed(slots, ".Data")
    prototype <- ClassDef@prototype
    ## check for a formal prototype object (TODO:  sometime ensure that this happens
    ## at setClass() time, so prototype slot in classRepresentation can have that class
    if(!.identC(class(prototype), className) && .isPrototype(prototype)) {
        pnames <- prototype@slots
        prototype <- prototype@object
    }
    else
        pnames <- names(attributes(prototype))
    if(length(slots) == 0 && !is.null(prototype))
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
            else if(length(slots) > 0) {
                for(slotName in slotsi) {
                    if(identical(slotName, ".Data")) {
                        if(!dataPartDone) {
                            prototype <- setDataPart(prototype, getDataPart(pri))
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
            else if(!dataPartDone && extends(cli, dataPartClass))
                prototype <- setDataPart(prototype, pri)
        }
    }
    if(length(slots) == 0)
        return(prototype)
    if(is.null(prototype))
        prototype <- defaultPrototype()
    pnames <- names(attributes(prototype))
    ## watch out for a prototype of this class.  Not supposed to happen, but will
    ## at least for the basic class "ts", and can lead to inf. recursion
    if(.identC(class(prototype), className))
        pslots <- names(attributes(unclass(prototype)))
    else if(isClass(class(prototype)))
        pslots <- names(getSlots(getClass(class(prototype))))
    else
        pslots <- NULL
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
                stop(gettextf("in constructing the prototype for class \"%s\": prototype has class \"%s\", but the data part specifies class \"%s\"",
                              className, .class1(prototype), dataPartClass),
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
    if(length(extra)>0 && is.na(match("oldClass", supers)))
        warning(gettextf("in constructing the prototype for class \"%s\", slots in prototype and not in class: %s",
                         className, paste(extra, collapse=", ")), domain = NA)
    ## now check the elements of the prototype against the class definition
    slotDefs <- getSlots(ClassDef); slotNames <- names(slotDefs)
    pnames <- names(attributes(prototype))
    pnames <- pnames[!is.na(match(pnames, slotNames))]
    check <- rep(FALSE, length(pnames))
    for(what in pnames) {
        pwhat <- slot(prototype, what)
        slotClass <- getClassDef(slotDefs[[what]], where)
        if(is.null(slotClass) || !extends(class(pwhat), slotClass)) {
            if(is.null(pwhat)) {
#                 warning("In class \"", className,
#                         "\", the prototype for slot \"", what, "\" (slot class \"",
#                         slotDefs[[what]],
#                         "\") is NULL; new() will fail for this class unless this slot is supplied in the call")
            }
            else
                check[match(what, pnames)] <- TRUE
        }
    }
    if(any(check))
        stop(gettextf("in making the prototype for class \"%s\" elements of the prototype failed to match the corresponding slot class: %s",
                      className,
                      paste(pnames[check], "(class", dQuote(slotDefs[match(pnames[check], slotNames)]), ")", collapse = ", ")), domain = NA)
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
    extends <- if(doExtends) completeExtends(ClassDef, where = where) else ClassDef@contains
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
        warning(gettextf("potential cycle in class inheritance: \"%s\" has duplicates in superclasses and subclasses (%s)",
                         Class, paste(bad, collapse = ", ")), domain = NA)
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
        if(length(ext) > 0) {
            superProps <- vector("list", length(ext)+1)
            superProps[[1]] <- properties
            for(i in seq_along(ext)) {
                eClass <- ext[[i]]
                if(isClass(eClass, where = where))
                    superProps[[i+1]] <- getClassDef(eClass, where = where)@slots
            }
            properties <- unlist(superProps, recursive = FALSE)
            ## check for conflicting slot names
            if(any(duplicated(allNames(properties)))) {
                duped <- duplicated(names(properties))
#TEMPORARY -- until classes are completed in place & we have way to match non-inherited slots
                properties <- properties[!duped]
#                 dupNames <- unique(names(properties)[duped])
#                 if(!is.na(match(".Data", dupNames))) {
#                     dataParts <- seq(along=properties)[names(properties) == ".Data"]
#                     dupNames <- dupNames[dupNames != ".Data"]
#                     ## inherited data part classes are OK but should be consistent
#                     dataPartClasses <- unique(as.character(properties[dataParts]))
#                     if(length(dataPartClasses)>1)
#                         warning("Inconsistent data part classes inherited (",
#                                 paste(dataPartClasses, collapse = ", "),
#                                 "): coercion to some may fail")
#                     ## remove all but the first .Data
#                     properties <- properties[-dataParts[-1]]
#                 }
#                 if(length(dupNames)>0) {
#                     dupClasses <- logical(length(superProps))
#                     for(i in seq(along = superProps)) {
#                         dupClasses[i] <- !all(is.na(match(dupNames, names(superProps[[i]]))))
#                     }
#                     stop(paste("Duplicate slot names: slots ",
#                                paste(dupNames, collapse =", "), "; see classes ",
#                                paste(c(Class, ext)[dupClasses], collapse = ", "), sep=""))
#                }
            }
        }
        ## ensure that each element of the slots is a valid class reference
        undefClasses <- rep(FALSE, length(properties))
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
            warning(gettextf("undefined slot classes in definition of \"%s\": %s",
                             ClassDef@className,
                             paste(names(properties)[undefClasses], "(class \"",
                                   unlist(properties, recursive = FALSE)[undefClasses],
                                   "\")", collapse = ", ", sep = "")),
                    domain = NA)
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
        if(length(prev)>0) {
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



getAllSuperClasses <-
  ## Get the names of all the classes that this class definition extends.
  ##
  ## A utility function used to complete a class definition.  It returns all the
  ## superclasses reachable from this class, in depth-first order (which is the order
  ## used for matching methods); that is, the first direct superclass followed by all its
  ## superclasses, then the next, etc.  (The order is relevant only in the case that
  ## some of the superclasses have multiple inheritance.)
  ##
  ## The list of superclasses is stored in the extends property of the session metadata.
  ## User code should not need to call getAllSuperClasses directly; instead, use getClass()@contains
  ## (which will complete the definition if necessary).
  function(ClassDef, simpleOnly = TRUE) {
    temp <- superClassDepth(ClassDef, simpleOnly = simpleOnly)
    unique(temp$label[sort.list(temp$depth)])
  }

superClassDepth <-
    ## all the superclasses of ClassDef, along with the depth of the relation
    ## Includes the extension definitions, but these are not currently used by
    ## getAllSuperClasses
  function(ClassDef, soFar = ClassDef@className, simpleOnly = TRUE )
{
    ext <- ClassDef@contains
    ## remove indirect and maybe non-simple superclasses (latter for inferring slots)
    ok <- rep(TRUE, length(ext))
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
    super <- list(label=immediate, depth = rep(1, length(immediate)), ext = ext)
    for(i  in seq_along(immediate)) {
        what <- immediate[[i]]
        if(!is.na(match(what, soFar)))
           ## watch out for loops (e.g., matrix/array have mutual is relationship)
           next
        exti <- ext[[i]]
        soFar <- c(soFar, what)
        if(!is(exti, "SClassExtension"))
            stop(gettextf("in definition of class \"%s\", information for superclass \"%s\" is of class \"%s\" (expected \"SClassExtension\")",
                          ClassDef@className, what, class(exti)), domain = NA)
        superClass <-  getClassDef(exti@superClass, package = exti@package)
            if(is.null(superClass)) {
                warning(gettextf("class \"%s\" extends an undefined class, \"%s\"",
                                 ClassDef@className, what), domain = NA)
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
            if(length(whatMore) > 0) {
                soFar <- c(soFar, whatMore)
                super$depth <- c(super$depth, 1+more$depth)
                super$label <- c(super$label, more$label)
                super$ext <- c(super$ext, more$ext)
            }
    }
    super
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
          stop(gettextf("trying to assign an object of class \"%s\" as the definition of class \"%s\": must supply a \"classRepresentation\" object",
                        class(def), Class), domain = NA)
      clName <- def@className; attributes(clName) <- NULL
      if(!.identC(Class, clName))
          stop(gettextf("assigning as \"%s\" a class representation with internal name \"%s\"",
                        Class, def@className), domain = NA)
      where <- as.environment(where)
      mname <- classMetaName(Class)
      if(exists(mname, envir = where, inherits = FALSE) && bindingIsLocked(mname, where)) {
          if(force)
            .assignOverBinding(mname, def, where, FALSE)
          else
            stop(gettextf("Class \"%s\" has a locked definition in package \"%s\"",
                          Class, getPackageName(where)))
      }
      else
          assign(mname, def, where)
      .cacheClass(clName, def, is(def, "ClassUnionRepresentation"))
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
}

.classNameFromMethods <- function(what) {
    packageSlot(what) <- "methods"
    what
  }

.initClassSupport <- function(where) {
    setClass("classPrototypeDef", representation(object = "ANY", slots = "character", dataPart = "logical"),
             sealed = TRUE, where = where)
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
               "double" =,
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
               "array" = (if(length(list(...)) > 0) array(...) else structure(numeric(), .Dim =0)),
               "matrix" = (if (length(list(...)) > 0) matrix(...) else matrix(0, 0, 0)),
#               "ts" = ts(...),
# break dependence on package stats
               "ts" = (if(length(list(...))) stats::ts(...)
               else structure(NA, .Tsp = c(1, 1, 1), class = "ts")),
                  {
                      args <- list(...)
                      if(length(args) == 1 && is(args[[1]], Class)) {
                          value <- as(args[[1]], Class)
                      }
                      else if(is.na(match(Class, .BasicClasses)))
                          msg <- paste("Calling new() on an undefined and non-basic class (\"",
                               Class, "\")", sep="")
                      else
                          msg <- gettextf("initializing objects from class \"%s\" with these arguments is not supported", Class)
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
     pp <- .Call("Rf_allocS4Object",PACKAGE="methods")
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
      if(!is.null(dataPartClass) && is.null(.validDataPartClass(dataPartClass, name)))
          stop(gettextf("in defining class \"%s\", the supplied data part class, \"%s\" is not valid (must be a basic class or a virtual class combining basic classes)", name, dataPartClass), domain = NA)
      prototypeClass <- getClass(class(prototype), where = where)
      if((!is.null(dataPartClass) || length(superClasses) > 0)
         && is.na(match("VIRTUAL", superClasses))) {
          ## Look for a data part in the superclasses, either an inherited
          ## .Data slot, or a basic class.  Uses the first possibility, warns of conflicts
          for(cl in superClasses) {
              clDef <- getClassDef(cl, where = where)
              thisDataPart <-  .validDataPartClass(clDef, name)
              if(!is.null(thisDataPart)) {
                  if(is.null(dataPartClass)) {
                      if(!is.na(match(thisDataPart, c("NULL", "environment"))))
                          warning(gettextf("class \"%s\" cannot be used as the data part of another class",
                                           thisDataPart), domain = NA)
                      else {
                          dataPartClass <- thisDataPart
                          if(!is.null(clDef@prototype)) {
                            newObject <- clDef@prototype
                            dataPartValue <- TRUE
                          }
                      }
                  }
                  else if(!extends(dataPartClass, thisDataPart) &&
                          !isVirtualClass(thisDataPart, where = where))
                      warning(gettextf("more than one possible class for the data part: using \"%s\" rather than \"%s\"",
                                       dataPartClass, thisDataPart),
                              domain = NA)
              }
          }
          if(length(dataPartClass) > 0) {
              if(is.na(match(".Data", slots))) {
                  properties <- c(list(".Data"= dataPartClass), properties)
                  slots <- names(properties)
              }
              else if(!extends(elNamed(properties, ".Data"), dataPartClass))
                  stop(gettextf("conflicting definition of data part: .Data = \"%s\", superclass implies \"%s\"",
                                elNamed(properties, ".Data"), dataPartClass),
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
                          stop(gettextf("a prototype object was supplied with object slot of class \"%s\", but the class definition requires an object that is class \"%s\"",
                                        class(prototype@object), dataPartClass),
                               domain = NA)
                  }
                  else if(!extends(prototypeClass, dataPartClass))
                      stop(gettextf("a prototype was supplied of class \"%s\", but the class definition requires an object that is class \"%s\"",
                                    class(prototype), dataPartClass),
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
                      stop(gettextf("slot \"%s\" in class \"%s\" currently defined (or inherited) as \"%s\", conflicts with an inherited definition in class \"%s\"",
                                    dup, name, elNamed(allProps, dup), cl),
                           domain = NA)
              theseSlots <- theseSlots[!dups]
              if(length(theseSlots)>0)
                  allProps[theseSlots] <- theseProperties[theseSlots]
          }
          else
              stop(gettextf("class \"%s\" extends an undefined class (\"%s\")",
                            name, cl), domain = NA)
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
          if((is.na(match(dataPartClass, .BasicClasses)) &&
             !isVirtualClass(dataPartDef)) ||
             length(dataPartDef@slots) > 0)
              stop(gettextf("\"%s\" is not eligible to be the data part of another class (must be a basic class or a virtual class with no slots)", dataPartClass),
                   domain = NA)
          if(extends(prototypeClass, "classPrototypeDef"))
          {}
          else if(extends(prototypeClass, dataPartClass)) {
              if(extends(prototypeClass, "list") && length(names(prototype)) > 0)
                  warning("prototype is a list with named elements (could be ambiguous):  better to use function prototype() to avoid trouble.")
          }
          else if(is.list(prototype))
              prototype <- do.call("prototype", prototype)
      }
      ## pnames will be the names explicitly defined in the prototype
      if(extends(prototypeClass, "classPrototypeDef")) {
          pnames <- prototype@slots
          prototype <- prototype@object
          if(length(superClasses) == 0 && any(is.na(match(pnames, slots))))
            stop(gettextf("named elements of prototype do not correspond to slot names: %s",
                      paste(dQuote(pnames[is.na(match(pnames, slots))]),
                            collapse =", ")))
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
  ## Tries to generate a new element from this class, but if the attempt fails
  ## (as, e.g., when the class is undefined or virtual) just returns NULL.
  ##
  ## This is inefficient and also not a good idea when actually generating objects,
  ## but is useful in the initial definition of classes.
  function(Class, where)
{
    ClassDef <- getClassDef(Class, where)
    if(is.null(ClassDef) || isVirtualClass(ClassDef))
        return(NULL)
    value <- trySilent(new(ClassDef))
    if(is(value, "try-error"))
        NULL
    else
        value
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
    if(identical(ClassDef@virtual, TRUE))
        cat("Virtual Class\n")
    x <- ClassDef@slots
    if(length(x)>0) {
        n <- length(x)
        cat("\n",propertiesAreCalled, ":\n", sep="")
        text <- format(c(names(x), as.character(x)), justify="right")
        text <- matrix(text, nrow =2, ncol = n, byrow = TRUE)
        dimnames(text) <- list(c("Name:", "Class:"), rep("", n))
        print(text, quote = FALSE)
    }
    else
        cat("\nNo ", propertiesAreCalled, ", prototype of class \"",
            .class1(ClassDef@prototype), "\"\n", sep="")
    ext <- ClassDef@contains
    if(length(ext)>0) {
        cat("\nExtends: ")
        showExtends(ext)
    }
    ext <- ClassDef@subclasses
    if(length(ext)>0) {
        cat("\nKnown Subclasses: ")
        showExtends(ext)
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
                if(length(eli@by) > 0)
                    paste("by class", paste("\"", eli@by,
                      "\", distance ",  eli@distance, sep="", collapse = ", "))
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
                    how[i] <- paste(how[i], ", with explicit coerce", sep="")
            }
        }
    }
    if(identical(printTo, FALSE))
        list(what = what, how = how)
    else if(all(!nzchar(how)) ||  all(how == "directly")) {
        what <- paste('"', what, '"', sep="")
        if(length(what)>1)
            what <- c(paste(what[-length(what)], ",", sep=""), what[[length(what)]])
        cat(file = printTo, what, fill=TRUE)
    }
    else cat(file = printTo, "\n", paste("Class \"", what, "\", ", how, "\n", sep=""), sep="")
}



print.classRepresentation <-
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
    if(.identC(class1[[1]], class2) || .identC(class2, "ANY"))
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
            if(!.identC(class(ClassDef2), "classRepresentation") &&
               isClassUnion(ClassDef2))
                ## a simple TRUE iff class1 or one of its superclasses belongs to the union
                i <- any(duplicated(c(class1, unique(nm1), names(ext))))
            else {
                i <- match(class1, names(ext))
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
    exts <- .walkClassGraph(ClassDef, "contains", where)
    if(length(exts)>0) {
        ## sort the extends information by depth (required for method dispatch)
        superClassNames <- getAllSuperClasses(ClassDef, FALSE)
        ## FIXME:  getAllSuperClassses sometimes misses.  Why?
        if(length(superClassNames) == length(exts))
            exts <- exts[superClassNames]
    }
    if(!missing(class2) && length(ClassDef@subclasses) > 0) {
        subclasses <-
            .transitiveSubclasses(ClassDef@className, class2, extensionDef, ClassDef@subclasses)
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
    if(!missing(class2) && length(classDef@contains) > 0) {
        contains <-
            .transitiveExtends(class2, classDef@className, extensionDef, classDef@contains)
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
.walkClassGraph <-  function(ClassDef, slotName, where)
{
    ext <- slot(ClassDef, slotName)
    className <- ClassDef@className
    ## the super- vs sub-class is identified by the slotName
    superClassCase <- identical(slotName, "contains")
    fromTo <- ClassDef@className
    what <- names(ext)
    for(i in seq_along(ext)) {
        by <- what[[i]]
        if(isClass(by, where = where)) {
            byDef <- getClass(by, where = where)
            exti <-  slot(byDef, slotName)
            ## add in those classes not already known to be super/subclasses
            exti <- exti[is.na(match(names(exti), what))]
            if(length(exti)> 0) {
                if(superClassCase)
                    exti <- .transitiveExtends(fromTo, by, ext[[i]], exti)
                else
                    exti <- .transitiveSubclasses(by, fromTo, ext[[i]], exti)
                ext <- c(ext, exti)
            }
        }
        else
            stop(gettextf("the \"%s\" list for class \"%s\", includes an undefined class \"%s\"",
                          if(superClassCase) "superClass" else "subClass",
                          className,.className(by)),
                 domain = NA)
    }
    what <- names(ext)  ## the direct and indirect extensions
    if(!all(is.na(match(what, className)))) {
        ok <- is.na(match(what, className))
        ## A class may not contain itself, directly or indirectly
        ## but a non-simple cyclic relation, involving setIs, is allowed
        for(i in seq_along(what)[!ok]) {
            exti <- ext[[i]]
            simple <- exti@simple
            if(simple) {
                fromDef <- getClassDef(exti@superClass, package = exti@package)
                extBack <- elNamed(slot(fromDef, slotName), className)
                simple <- is(extBack, "SClassExtension") && extBack@simple
            }
            if(simple) {
                if(superClassCase) {
                    whatError <-  "contain itself"
                    relation <- "contains"
                }
                else {
                    whatError <- "have itself as a subclass"
                    relation <- "has subclass"
                }
                ## this is not translatable
                stop(sprintf("class \"%s\" may not %s: it %s class \"%s\", with a circular relation back to \"%s\"",
                              className, whatError, relation, fromTo, className),
                     domain = NA)
            }
        }
        ## but sub/superclasses can enter multiple ways, with all but the first
        ## ignored.
        ext <- ext[ok]
    }
    ext
}


classMetaName <-
  ## a name for the object storing this class's definition
  function(name)
  methodsPackageMetaName("C", name)

##FIXME:  C code should take multiple strings in name so the paste() call in
## mlistMetaName, etc. could be avoided.
methodsPackageMetaName <-
  ## a name mangling device to simulate the meta-data in S4
  function(prefix, name)
  ## paste(".", prefix, name, sep="__") # too slow
    .Call("R_methodsPackageMetaName", prefix, name, PACKAGE = "methods")



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
}

## Construct an error message for an unsatisfied required method.
.missingMethod <- function(f, message = "", method) {
    if(nzchar(message))
        message <- paste("(", message, ")", sep="")
    message <- paste("for function", f, message)
    if(is(method, "MethodDefinition")) {
        target <-  paste(dQuote(method@target), collapse=", ")
        defined <- paste(dQuote(method@defined), collapse=", ")
        message <- paste("Required method", message, "not defined for signature",
                         target)
        if(!identical(target, defined))
            message <- paste(message, ", required for signature", defined)
    }
    else message <- paste("Required method not defined", message)
    message
}

getSlots <- function(x) {
    if(isClassDef(x))
        classDef <- x
    else
        classDef <- getClass(x)
    props <- classDef@slots
    value <- as.character(props)
    names(value) <- names(props)
    value
}


## check for reserved slot names.  Currently only "class" is reserved
validSlotNames <- function(names) {
    i <- match("class", names)
    if(is.na(i))
        names
    else
        stop("\"class\" is a reserved slot name and cannot be redefined")
}

### utility function called from primitive code for "@"
getDataPart <- function(object) {
    temp <- getClass(class(object))@slots
    if(length(temp) == 0)
        return(object)
    if(is.na(match(".Data", names(temp))))
       stop(gettextf("no '.Data' slot defined for class \"%s\"", class(object)),
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

setDataPart <- function(object, value) {
    classDef <- getClass(class(object))
    dataClass <- elNamed(getSlots(classDef), ".Data")
    if(is.null(dataClass))
        stop(gettextf("class \"%s\" does not have a data part (a .Data slot) defined",
                      class(object)), domain = NA)
    value <- as(value, dataClass)
    .mergeAttrs(value, object)
}

.validDataPartClass <- function(cl, inClass) {
    if(is(cl, "classRepresentation")) {
        ClassDef <- cl
        cl <- ClassDef@className
    }
    else
        ClassDef <- getClass(cl, TRUE)

    switch(cl, ts =, matrix = , array = value <- cl,
           value <- elNamed(ClassDef@slots, ".Data"))
    if(is.null(value)) {
        if(.identC(cl, "structure"))
            value <- "vector"
        else if((extends(cl, "vector") || !is.na(match(cl, .BasicClasses))))
            value <- cl
        else if(extends(cl, "oldClass") && isVirtualClass(cl)) {
            ## The following warning is obsolete if S3 classes can be
            ## non-virtual--the subclass can have a prototype

##             else
##                 warning(gettextf("old-style ('S3') class \"%s\" supplied as a superclass of \"%s\", but no automatic conversion will be peformed for S3 classes",
##                                  cl, .className(inClass)), domain = NA)
        }
        else if(identical(ClassDef@virtual, TRUE) &&
               length(ClassDef@slots) == 0 &&
               length(ClassDef@subclasses) > 0 ) {
                ## look for a union of basic classes
                subclasses <- ClassDef@subclasses
                what <- names(subclasses)
                value <- cl
                for(i in seq_along(what)) {
                    ext <- subclasses[[i]]
                    ##TODO:  the following heuristic test for an "original"
                    ## subclass should be replaced by a suitable class (extending SClassExtension)
                    if(length(ext@by) == 0 && ext@simple && !ext@dataPart &&
                       is.na(match(what[i], .BasicClasses))) {
                        value <- NULL
                        break
                    }
                }
            }
    }
    value
}

.mergeAttrs <- function(value, object, explicit = NULL) {
    supplied <- attributes(object)
    if(length(explicit)>0)
        supplied[names(explicit)] <- explicit
    valueAttrs <- attributes(value)
    ## names are special.
    if(length(supplied$names)>0 && length(valueAttrs$names) == 0) {
        if(length(value) != length(object))
            length(supplied$names) <- length(value)
    }
    if(length(valueAttrs) == 0) # nothing to protect
        attributes(value) <- supplied
    else {
        valueAttrs$class <- NULL # copy in class if it's supplied
           # otherwise, don't overwrite existing attrs
        for(what in names(supplied))
            if(is.null(valueAttrs[[what]]))
                attr(value, what) <- supplied[[what]]
    }
    value
}

.newExternalptr <- function()
    .Call("R_externalptr_prototype_object", PACKAGE = "methods")

## modify the list moreExts, currently from class `by', to represent
## extensions instead from an originating class; byExt is the extension
## from that class to `by'
.transitiveExtends <- function(from, by, byExt, moreExts) {
    what <- names(moreExts)
    for(i in seq_along(moreExts)) {
        toExt <- moreExts[[i]]
        to <- what[[i]]
        toExt <- .combineExtends(byExt, toExt, by, to)
        moreExts[[i]] <- toExt
    }
    moreExts
}

.transitiveSubclasses <- function(by, to, toExt, moreExts) {
    what <- names(moreExts)
    for(i in seq_along(moreExts)) {
        byExt <- moreExts[[i]]
        byExt <- .combineExtends(byExt, toExt, by, to)
        moreExts[[i]] <- byExt
    }
    moreExts
}

.combineExtends <- function(byExt, toExt, by, to) {
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
        ## Is there a danger of infinite loop below?
        expr <- substitute({.value <- as(from, BY); as(.value, TO) <- value; value <- .value; BYEXPR},
                           list(BY=by, TO = to, BYEXPR = byExpr))
        body(f, envir = environment(f)) <- expr
        toExt@replace <- f
        toExt@by <- toExt@subClass
        toExt@subClass <- byExt@subClass
        toExt@distance <- toExt@distance + byExt@distance
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
    if(sameSlots)
        expr <- substitute({class(from)[[1]] <- CLASS; from},
                   list(CLASS = toClass))
    else {
        if(length(toSlots)==0) {
            ## either a basic class or something with the same representation
            if(is.na(match(toClass, .BasicClasses)))
                expr <- substitute({ attributes(from) <- NULL; class(from)[[1]] <- CLASS; from},
                                   list(CLASS=toClass))
            else if(isVirtualClass(toDef))
                expr <- quote(from)
            else {
                ## a basic class; a vector type, matrix, array, or ts
                switch(toClass,
                       matrix = , array = {
                           expr <- quote({.dm <- dim(from); .dn <- dimnames(from)
                                    attributes(from) <- NULL; dim(from) <- .dm
                                    dimnames(from) <- .dn; from})
                       },
                       ts = {
                           expr <- quote({.tsp <- tsp(from); attributes(from) <- NULL
                                          tsp(from) <- .tsp; class(from) <- "ts"; from})
                       },
                       expr <- quote({attributes(from) <- NULL; from})
                       )
            }
        }
        else {
            expr <- substitute({
            value <- new(CLASS)
            for(what in TOSLOTS)
                slot(value, what) <- slot(from, what)
            value }, list(CLASS=toClass, TOSLOTS = toSlots))
        }
    }
    expr
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

## the real version of newClassRepresentation, assigned in .First.lib
.newClassRepresentation <- function(...)
    new("classRepresentation", ...)

.insertExpr <- function(expr, el) {
    if(!is(expr, "{"))
        expr <- substitute({EXPR}, list(EXPR = expr))
    expr[3:(length(expr)+1)] <- expr[2:length(expr)]
    expr[[2]] <- el
    expr
}

## utility guaranteed to return only the first string of the class.
## Would not be needed if we dis-allowed S3 classes with multiple strings (or
## if the methods package version of class dropped the extra strings).
.class1 <- function(x) {
    cl <- class(x)
    if(length(cl)>1)
        cl[[1]]
    else
        cl
}

substituteFunctionArgs <- function(def, newArgs, args = formalArgs(def), silent = FALSE, functionName = "a function") {
    if(!identical(args, newArgs)) {
        if( !missing(functionName) )
            functionName = paste("for", functionName)
        
        n <- length(args)
        if(n != length(newArgs))
            stop(gettextf("trying to change the argument list of %s with %d arguments to have arguments (%s)",
                          functionName, n, paste(newArgs, collapse = ", ")),
                 domain = NA)
        bdy <- body(def)
        ## check for other uses of newArgs
        checkFor <- newArgs[is.na(match(newArgs, args))]
        locals <- all.vars(bdy)
        if(length(checkFor) > 0 && any(!is.na(match(checkFor, locals))))
            stop(gettextf("get rid of variables in definition %s (%s); they conflict with the needed change to argument names (%s)",
                          functionName,
                          paste(checkFor[!is.na(match(checkFor, locals))], collapse = ", "),
                          paste(newArgs, collapse = ", ")), domain = NA)
        ll <- vector("list", 2*n)
        for(i in seq_len(n)) {
            ll[[i]] <- as.name(args[[i]])
            ll[[n+i]] <- as.name(newArgs[[i]])
        }
        names(ll) <- c(args, newArgs)
        body(def, envir = environment(def)) <- substituteDirect(bdy, ll)
        if(!silent) {
            msg <-
                gettextf("arguments in definition %s changed from (%s) to (%s)",
                         functionName,
                         paste(args, collapse = ", "),
                         paste(newArgs, collapse = ", "))
            message(strwrap(msg), domain = NA)
        }
    }
    def
}

.makeValidityMethod <- function(Class, validity) {
    if(!is.null(validity)) {
        if(!is(validity, "function"))
            stop(gettextf("a validity method must be a function of one argument, got an object of class \"%s\"", class(validity)), domain = NA)
        validity <- substituteFunctionArgs(validity, "object", functionName = paste("validity method for class", Class))
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
    if(is.character(package)) {
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
    if(!require(package, character.only = TRUE)) {
        if(mustFind)
          stop(gettextf("unable to find required package \"%s\"", package),
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
        if(is.na(m))
            stop(gettextf("Package \"%s\" is not loaded", package), domain = NA)
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


..classEnv <- function(Class, default = .requirePackage("methods"), mustFind = TRUE) {
    if(is.character(Class))
        package <- packageSlot(Class)
    else ## must then be a class definition
        package <- Class@package
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
                stop(gettextf("unable to find an environment containing class \"%s\"", Class), domain = NA)
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

.cacheClass <- function(name, def, doSubclasses = FALSE) {
    if(!identical(doSubclasses, FALSE))
      .recacheSubclasses(def@className, def, doSubclasses)
    if(exists(name, envir = .classTable, inherits = FALSE)) {
        newpkg <- def@package
        prev <- get(name, envir = .classTable)
        if(is(prev, "classRepresentation")) {
            if(identical(prev, def))
               return()
            pkg <- prev@package # start a per-package list
            if(identical(pkg, newpkg)) # redefinition
              return(assign(name, def, envir = .classTable))
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
    }
    assign(name, def, envir = .classTable)
}

.uncacheClass <- function(name, def) {
    if(exists(name, envir = .classTable, inherits = FALSE)) {
        newpkg <- def@package
        prev <- get(name, envir = .classTable)
        if(is(prev, "classRepresentation"))  # we might worry if  prev not identical?
            return(remove(list = name, envir = .classTable))
         i <- match(newpkg, names(prev))
        if(!is.na(i))
           prev[[i]] <- NULL
        else # we might warn about unchaching more than once
          return()
        if(length(prev) == 0)
          return(remove(list = name, envir = .classTable))
        else if(length(prev) == 1)
          prev <- prev[[1]]
        assign(name, prev, envir  = .classTable)
    }
}

.getClassFromCache <- function(name, where) {
    if(exists(name, envir = .classTable, inherits = FALSE)) {
        value <- get(name, envir = .classTable)
        if(is.list(value)) { # multiple classes with this name
            pkg <- packageSlot(name)
            if(is.null(pkg) && is.character(where))
              pkg <- where
            else
              pkg <- getPackageName(where)
            pkgs <- names(value)
            i <- match(pkg, pkgs,0)
            if(i > 0)
              return(value[[i]])
            i <- match("methods", pkgs,0)
            if(i > 0)
               return(value[[i]])
            else
              return(NULL)
        }
        value
    }
    else
      NULL
}

### insert superclass information into all the subclasses of this
### class.  Used to incorporate inheritance information from
### ClassUnions
.recacheSubclasses <- function(class, def, subclasses) {
    if(identical(subclasses, TRUE))
      subclasses <- class
    subs <- def@subclasses
    subNames <- names(subs)
    for(i in seq_along(subs)) {
        what <- subNames[[i]]
        subDef <- getClassDef(what)
        if(is.null(subDef))
          warning(
           gettextf("Undefined subclass, \"%s\", of class \"%s\"; definition not updated",
                    what, def@className))
        else if(match(what, subclasses, 0) > 0)
          next # would like warning, but seems to occur often
          #warning(
             #gettextf("Apparent loop in subclasses: \"%s\" found twice; ignored this time", what))
        else if(is.na(match(what, names(subDef@contains)))) {
            subclasses <- c(subclasses, what)
            subDef@contains[[class]] <- subs[[i]]
            .cacheClass(what, subDef, subclasses)
        }
    }
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
          warning(
             gettextf("Subclass \"%s\" of class \"%s\" is not local and cannot be updated for new inheritance information; consider setClassUnion()",
                      what, class))
          next
        }
        extension <- extDefs[[what]]
        if(is.null(extension)) # not possible if the setIs behaved?
          warning(
              gettextf("No definition of inheritance from \"%s\" to \"%s\", though the relation was implied by the setIs() from \"%s\"",
                       what, def2@className, class))
        else if(is.na(match(class2, names(subDef@contains)))) {
            subDef@contains[[class2]] <- extension
            assignClassDef(what, subDef, cwhere, TRUE)
        }
    }
}

.removeSuperclassBackRefs <- function(Class, classDef, classWhere) {
    if(length(classDef@contains)>0) {
        superclasses <- names(classDef@contains)
        for(what in superclasses) {
            superWhere <- findClass(what, classWhere)
            if(length(superWhere)>0) {
                superWhere <- superWhere[[1]]
                .removeSubClass(what, Class, superWhere)
            }
            else
              warning(gettextf("Couldn't find superclass \"%s\" to clean up when removing subclass references to class \"%s\"",
                               what, Class))
        }
    }
}


## remove subclass from the known subclasses of class
## both in the package environment and in the cache
.removeSubClass <- function(class, subclass, where) {
    mname <- classMetaName(class)
    where <- as.environment(where)
    if(exists(mname, envir = where, inherits = FALSE)) {
        cdef <- get(mname, envir = where)
        newdef <- .deleteSubClass(cdef, subclass)
        if(!is.null(newdef))
          assignClassDef(class, newdef,  where, TRUE)
        else { # check the cache
            cdef <- .getClassFromCache(cdef@className, where)
            if(is.null(cdef)) {}
            else {
                newdef <- .deleteSubClass(cdef, subclass)
                if(!is.null(newdef))
                  .cacheClass(class, newdef)
            }
        }
        sig <- signature(from=subclass, to=class)
        if(existsMethod("coerce", sig))
          .removeCachedMethod("coerce", sig)
        if(existsMethod("coerce<-", sig))
          .removeCachedMethod("coerce<-", sig)
        .uncacheClass(class, cdef)
    }
    else
      warning(gettextf("No class \"%s\" found as expected in removing subclass \"%s\"",
                       class, subclass))
}

.deleteSubClass <- function(cdef, subclass) {
        subclasses <- cdef@subclasses
        ii <- match(subclass, names(subclasses), 0)
        ## the subclass may not be there, e.g., if an error occured in
        ## setClass, or (in 2.4.0) if class is sealed
        if(ii > 0) {
            cdef@subclasses <- subclasses[-ii]
            cdef
        }
        else
          NULL
    }
