
testVirtual <-
  ## Test for a Virtual Class.
  ## Figures out, as well as possible, whether the class with these properties,
  ## extension, and prototype is a virtual class.
  ## Can be forced to be virtual by extending "VIRTUAL".  Otherwise, a class is
  ## virtual only if it has no slots, extends no non-virtual classes, and has a
  ## NULL Prototype
  function(properties, extends, prototype)
{
    if(length(extends)) {
        en <- names(extends)
        if(!is.na(match("VIRTUAL", en)))
            return(TRUE)
        ## does the class extend a known non-virtual class?
        for(what in en)
            if(isClass(what) && identical(getVirtual(getClass(what)), FALSE))
                return(FALSE)
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
  function(properties, prototype, extends)
{
    if(length(properties) == 0 && !is.null(prototype))
            return(prototype)
    ## try for a single superclass that is not virtual
    supers <- names(extends)
    virtual <- NA
    needsPrototype <- is.null(prototype)
    for(i in seq(along=extends)) {
        if(!is.logical(el(extends, i)))
            next
        what <- el(supers, i)
        if(identical(what, "VIRTUAL"))
            ## the class is virtual, and the prototype usually NULL
            virtual <- TRUE
        else if(needsPrototype && isClass(what) && !isVirtualClass(what)) {
            if(is.null(prototype)) {
                prototype <- getPrototype(getClass(what))
                fromClass <- what
            }
            else {
                ## two super classes: check "data part" for consistency
                ## (conflicting slot names are checked elsewhere)
                pOther <- getPrototype(getClass(what))
                attributes(pOther) <- NULL
                pThis <- prototype
                attributes(pThis) <- NULL
                if(!identical(pOther, pThis) && !identical(pThis, list()))
                    warning("classes \"", what, "\" and \"", fromClass,
                            "\" have different prototypes; using the one from \"",
                            fromClass, "\"")
            }
        }
    }
    if(length(properties) == 0)
        return(prototype)
    if(is.null(prototype))
        prototype <- defaultPrototype()
    pnames <- names(attributes(prototype))
    pslots <- if(isClass(class(prototype))) names(getSlots(getClass(class(prototype)))) else
        NULL
    snames <- names(properties)
    if(!is.na(match(".Data", snames))) {
        dataPartClass <- elNamed(properties, ".Data")
        ## check the data part
        if(!(isVirtualClass(dataPartClass) || is(prototype, dataPartClass)))
            stop(paste("Prototype has class \"",
                       .class1(prototype), "\", but the data part specifies class \"",
                       dataPartClass,"\"", sep=""))
        snames <- snames[-match(".Data", snames)]
    }
    for(j in seq(along = snames)) {
        name <- el(snames, j)
        i <- match(name, pnames)
        if(is.na(i))
            slot(prototype, name, check=FALSE) <- tryNew(el(properties, j))
    }
    extra <- pnames[is.na(match(pnames, snames)) & !is.na(match(pnames, pslots))]
    if(length(extra)>0)
        warning(paste("Slots in prototype and not in class:",
                      paste(extra, collapse=", ")))
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
  ## Completes the definition of Class, relative to the current session.
  ##
  ## The completed definition is stored in the session's class metadata,
  ## to be retrieved the next time that getClass is called on this class,
  ## and is returned as the value of the call.
  function(Class, ClassDef = getClassDef(Class))
{
    if(isClass(Class) || !missing(ClassDef)) {
        properties <- getProperties(ClassDef)
        immediate <- .mergeExtends(extendsMetaName(ClassDef))
        simpleContains <- ClassDef@contains
        if(any(is.na(match(names(simpleContains), names(immediate))))) {
            bad <- names(simpleContains)[is.na(match(names(simpleContains), names(immediate)))]
            msg <- paste("inconsistent included class(es): ", paste(bad, collapse=", "),
                         sep = "")
            for(what in bad) {
                if(isClass(what))
                    warning("Class \"", what, "\" is included in class \"", Class, "\" but not in the extends metadata")
                else
                    stop(paste("Class \"", what, "\" is included in class \"", Class,
                               "\" but no longer defined", sep=""))
            }
            immediate <- simpleContains
        }
        ClassDef@contains <- immediate
        ext <- getAllSuperClasses(ClassDef)
        ## ext has the names of all the direct and indirect superClasses but NOT those that do
        ## an explicit coerce (we can't conclude anything about slots, etc. from them)
        if(length(ext) > 0) {
            superProps <- vector("list", length(ext)+1)
            superProps[[1]] <- properties
            for(i in seq(along=ext)) {
                eClass <- ext[[i]]
                if(isClass(eClass))
                    superProps[[i+1]] <- getProperties(getClassDef(eClass))
            }
            properties <- unlist(superProps, recursive = FALSE)
            ## check for conflicting slot names
            if(any(duplicated(allNames(properties)))) {
                duped <- duplicated(names(properties))
                dupNames <- unique(names(properties)[duped])
                if(!is.na(match(".Data", dupNames))) {
                    dataParts <- seq(along=properties)[names(properties) == ".Data"]
                    dupNames <- dupNames[dupNames != ".Data"]
                    ## inherited data part classes are OK but should be consistent
                    dataPartClasses <- unique(as.character(properties[dataParts]))
                    if(length(dataPartClasses)>1)
                        warning("Inconsistent data part classes inherited (",
                                paste(dataPartClasses, collapse = ", "),
                                "): coercion to some may fail")
                    ## remove all but the first .Data
                    properties <- properties[-dataParts[-1]]
                }
                if(length(dupNames)>0) {
                    dupClasses <- logical(length(superProps))
                    for(i in seq(along = superProps)) {
                        dupClasses[i] <- !all(is.na(match(dupNames, names(superProps[[i]]))))
                    }
                    stop(paste("Duplicate slot names: slots ",
                               paste(dupNames, collapse =", "), "; see classes ",
                               paste(c(Class, ext)[dupClasses], collapse = ", "), sep=""))
                }
            }
        }
        prototype <- makePrototypeFromClassDef(properties, getPrototype(ClassDef), immediate)
        virtual <- getVirtual(ClassDef)
        validity <- getValidity(ClassDef)
        access <- getAccess(ClassDef)
        package <- getClassPackage(ClassDef)
        ## assign a temporary list with some class information, but NOT a proper class
        ## representation.  Used to avoid infinite recursion if the extensions of the
        ## class hierarchy have loops (e.g., matrix <-> array)
        assignClassDef(Class, .tempClassDef(className = Class, slots = properties,
                                     contains = immediate,
                                     prototype = prototype,
                                     virtual = virtual,
                                     validity = validity,
                                     access = access,
                                     package = package), 0)
        on.exit(resetClass(Class, FALSE)) # in case of error, remove the temp. def'n
        extends <- completeExtends(ClassDef)
        subclasses <- .mergeExtends(subclassesMetaName(ClassDef))
        if(is.na(virtual))
            ## compute it from the immediate extensions, but all the properties
            virtual <- testVirtual(properties, immediate, prototype)
        ## modify the initial class definition object, rather than creating
        ## a new one, to allow extensions of "classRepresentation"
        ## Done by a separate function to allow a bootstrap version.
        ClassDef <- .mergeClassDefSlots(ClassDef,
        slots = properties,
        contains = extends,
        prototype = prototype,
        virtual = virtual,
        subclasses = subclasses)
    }
    else {
        ## create a class definition of an empty virtual class
        ClassDef <- newClassRepresentation(className = Class, virtual = TRUE)
    }
    assignClassDef(Class, ClassDef, 0)
    ## NOW complete the subclass information:  doing it before the assign
    ## would produce a recursive loop of calls to completeClassDefinition(Class)
    ClassDef@subclasses <- completeSubclasses(ClassDef)
    assignClassDef(Class, ClassDef, 0)
    if(any(!is.na(match(names(ClassDef@subclasses), names(ClassDef@contains))))
       && options()$warn > 0  ## NEEDED:  a better way to turn on strict testing
       ) {
        bad <- names(ClassDef@subclasses)[!is.na(match(names(ClassDef@subclasses), names(ClassDef@contains)))]
        warning("Potential cycle in class inheritance: \"",Class,
                "\" has duplicates in superclasses and subclasses (",
                paste(bad, collapse = ", "), ")")
    }
    on.exit() # clear the removal of the class definition 
    ClassDef
}


getProperties <-
  ## Extracts the class's Properties information from the class representation (only, not from
  ## the name of the class).
  function(ClassDef)
   ClassDef@slots

getExtends <-
  ## extract the class's Extends information from the class representation (only, not from
  ## the name of the class)
  ##
  ## Contrast with the `possibleExtends' and `is' functions, both of which use indirect
  ## information as well.
  function(ClassDef)
    ClassDef@contains

## merge the lists of either extends or subclass information from
## the corresponding metadata:  what is the corresponding metadata name
.mergeExtends <- function(what) {
    allWhere <- .findMetaData(what)
    value <-list()
    for(where in rev(allWhere)) {
        ext <- get(what, where)
        if(length(value)>0)
            value[names(ext)] <- ext
        else
            value <- ext
    }
    value
  }

getValidity <-
   ## extract the class's Validity method (or NULL) from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
    ClassDef@validity
 

getAccess <-
   ## extract the class's Access method (or NULL) from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
    ClassDef@access
 

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
  ## User code should not need to call getAllSuperClasses directly; instead, use getExtends(getClass())
  ## (which will complete the definition if necessary).
  function(ClassDef) {
    temp <- superClassDepth(ClassDef)
    unique(temp$label[sort.list(temp$depth)])
  }

superClassDepth <-
    ## all the (simple) superclasses of ClassDef, along with the depth of the relation
  function(ClassDef, soFar = getClassName(ClassDef) )
{
    ext <- getExtends(ClassDef)
    ## remove non-simple superclasses.  We can't use these
    ## to infer information about slots, etc.
    ok <- logical(length(ext))
    for(i in seq(along=ext))
        ok[i] <- ext[[i]]@simple
    ext <- ext[ok]
    immediate <- names(ext)
    ## watch out for loops (e.g., matrix/array have mutual is relationship)
    immediate <- immediate[is.na(match(immediate, soFar))]
    soFar <- c(soFar, immediate)
    super <- list(label=immediate, depth = rep(1, length(immediate)))
    for(what in immediate) {
        if(isClass(what)) {
            superClass <- getClassDef(what)
            if(is.null(superClass)) {
                warning("class \"", getClassName(ClassDef), "\" extends an undefined class,\"",
                        what, "\"")
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
                whatMore <- whatMore[ok]
            }
            if(length(whatMore) > 0) {
                soFar <- c(soFar, whatMore)
                super$depth <- c(super$depth, 1+more$depth)
                super$label <- c(super$label, more$label)
            }
        }
        else
            warning("Class information incomplete: class \"",
                    what, "\" not defined")
    }
    super
}

setExtendsMetaData <-
  ## save the metadata defining this extends relationship
  function(ClassDef1, ClassDef2, value, where) {
      what <- extendsMetaName(ClassDef1)
      if(exists(what, where, inherits = FALSE))
          obj <- get(what, where)
      else
          obj <- list()  ## reallly listOf("SClassExtension")
      elNamed(obj, getClassName(ClassDef2)) <- value
      assign(what, obj, where)
  }
      
setSubclassMetaData <-
  ## save the metadata defining this sublcass relation
  function(ClassDef1, ClassDef2, where) {
      what <- subclassesMetaName(ClassDef1)
      if(exists(what, where, inherits = FALSE))
          obj <- get(what, where)
      else
          obj <- list()
      subClass <- getClassName(ClassDef2)
      elNamed(obj, subClass) <- c(ClassDef1@className, ClassDef1@package)
      assign(what, obj, where)
      obj[[subClass]] # return subclass info; used by setIs to update cached class def'n
  }

getPrototype <-
  ## extract the class's Prototype information from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
  ClassDef@prototype

getVirtual <-
  ## extract the class's Virtual information from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
    ClassDef@virtual

isVirtualClass <-
  ## Is the named class a virtual class?  A class is virtual if explicitly declared to
  ## be, and also if the class is not formally defined.
  function(Class) {
      if(isClassDef(Class))
          getVirtual(Class)
      else if(isClass(Class))
          getVirtual(getClass(Class))
      else
          TRUE
  }

getSubclasses <-
  ## extract the class's Subclasses information from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
    ClassDef@subclasses

getClassName <-
  ## The internal property in the class definition for the class name.
  function(ClassDef)
    ClassDef@className

getClassPackage <-
  ## The internal property in the class definition for the class package.
  function(ClassDef)
    ClassDef@package


assignClassDef <-
  ## assign the definition of the class to the specially named object
  function(Class, def, where = .GlobalEnv) {
    if(identical(where, 0))
      assignToClassMetaData(classMetaName(Class), def)
    else assign(classMetaName(Class), def, where)
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
    proto <- list()
    pnames <- names(protoSlots)
    for(i in seq(along=protoSlots))
        slot(proto, pnames[[i]], FALSE) <- protoSlots[[i]]
    class(proto) <- "classRepresentation"
    object <- list()
    class(object) <- "classRepresentation"
    slot(object, "slots", FALSE) <- defSlots
    slot(object, "className", FALSE) <- "classRepresentation"
    slot(object, "virtual", FALSE) <- FALSE
    slot(object, "prototype", FALSE) <- proto
    for(what in c("contains", "validity", "access", "hasValidity", "subclasses",
                  "versionKey"))
        slot(object, what, FALSE) <- elNamed(protoSlots, what)
    slot(object, "sealed", FALSE) <- TRUE
    slot(object, "package", FALSE) <- getPackageName(where)
    assignClassDef("classRepresentation", object, where)
    assignClassDef("classRepresentation", object, 0)
}

.initClassSupport <- function(where) {
    setClass("classPrototypeDef", representation(object = "ANY", slots = "character"),
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
               "list" =  as.vector(c(...), Class),
               "expression" = eval(substitute(expression(...))),
               "externalptr" = {
                   if(nargs() > 1)
                       stop("externalptr objects cannot be initialized from new()")
                   .newExternalptr()
               },
               "single" = as.single(c(...)),
                  ## note on array, matrix:  not possible to be compatible with
                  ## S-Plus on array, unless R allows 0-length .Dim attribute
               "array" = (if(length(list(...)) > 0) array(...) else structure(numeric(), .Dim =0)),
               "matrix" = (if (length(list(...)) > 0) matrix(...) else matrix(0, 0, 0)),
               "ts" = ts(...),
                  {
                      args <- list(...)
                      if(length(args) == 1 && is(args[[1]], Class)) {
                          value <- as(args[[1]], Class)
                      }
                      else if(is.na(match(Class, .BasicClasses)))
                          msg <- paste("Calling new() on an undefined and non-basic class (\"",
                               Class, "\")", sep="")
                      else
                          msg <- paste("Initializing objects from class \"", Class, "\" with these arguments is not supported",
                                     sep ="")
                  }
                  )
  if(is.null(msg))
      value
  else
      stop(msg)
}

defaultPrototype <-
    ## the starting prototype for a non-virtual class
    ## Should someday be a non-vector sexp type
    function()
    list()

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
  function(name, properties, prototype, superClasses) {
      ## the StandardPrototype should really be a type that doesn't behave like
      ## a vector.  But none of the existing SEXP types work.  Someday ...
      StandardPrototype <- defaultPrototype()
      slots <-  validSlotNames(allNames(properties))
      dataPartClass <- elNamed(properties, ".Data")
      if(!is.null(dataPartClass) && is.null(.validDataPartClass(dataPartClass)))
          stop("\"", dataPartClass, "\" is not a valid data part class (must be a basic class or a virtual class combining basic classes)")
      if((!is.null(dataPartClass) || length(superClasses) > 0)
         && is.na(match("VIRTUAL", superClasses))) {
          ## Look for a data part in the super classes, either an inherited
          ## .Data slot, or a basic class.  Uses the first possibility, warns of conflicts
          for(cl in superClasses) {
              thisDataPart <-  .validDataPartClass(cl)
              if(!is.null(thisDataPart)) {
                  if(is.null(dataPartClass)) {
                      if(!is.na(match(thisDataPart, c("NULL", "environment"))))
                          warning("Class \"", thisDataPart, "\" cannot be used as the data part of another class")
                      else
                          dataPartClass <- thisDataPart
                  }
                  else if(!extends(dataPartClass, thisDataPart) &&
                          !isVirtualClass(thisDataPart))
                      warning("More than one possible class for the data part:  using \"",
                              dataPartClass, "\" rather than \"",
                              thisDataPart, "\"")
              }
          }
          if(length(dataPartClass) > 0) {
              if(is.na(match(".Data", slots))) {
                  properties <- c(list(".Data"= dataPartClass), properties)
                  slots <- names(properties)
              }
              else if(!extends(elNamed(properties, ".Data"), dataPartClass))
                  stop(paste("Conflicting definition of data part: .Data = \"",
                             elNamed(properties, ".Data"),"\", super class implies \"",
                             dataPartClass, "\"", sep=""))
              pslots <- NULL
              if(is.null(prototype)) {
                  if(isVirtualClass(dataPartClass))
                      ## the equivalent of new("vector")
                      prototype <- newBasic("logical")
                  else
                      prototype <- new(dataPartClass)
              }
              else {
                  if(is(prototype, "classPrototypeDef"))
                      pobject <- prototype@object
                  else
                      pobject <- prototype
                  if(!is(pobject, dataPartClass))
                  stop(paste("Class of supplied prototype (\"",
                             class(pobject), "\") conflicts with the class of ",
                             " the data part (\"", dataPartClass, "\")",sep=""))
              }
          }
          if(is.null(prototype)) { ## non-vector (may extend NULL)
              prototype <- StandardPrototype
          }
      }
      ## check for conflicts in the slots
      allProps <- properties
      for(i in seq(along=superClasses)) {
          cl <- superClasses[[i]]
          if(isClass(cl)) {
              theseProperties <- getSlots(cl)
              theseSlots <- names(theseProperties)
              theseSlots <- theseSlots[theseSlots == ".Data"] # handled already
              dups <- !is.na(match(theseSlots, allProps))
              for(dup in theseSlots[dups])
                  if(!extends(elNamed(allProps, dup), elNamed(theseProperties, dup)))
                      stop(paste("Slot \"", dup, "\" in class \"", name,
                                 "\" currently defined (or inherited) as \"", elNamed(allProps, dup),
                                 "\", conflicts with an inherited definition in class \"",
                                 cl, "\"", sep=""))
              theseSlots <- theseSlots[!dups]
              if(length(theseSlots)>0)
                  allProps[theseSlots] <- theseProperties[theseSlots]
          }
          else
              stop(paste("Class \"", name, "\" extends an undefined class (\"",
                         cl, "\"", sep=""))
      }
      if(is.null(dataPartClass)) {
          if(is(prototype, "classPrototypeDef"))
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
              stop(paste("\"", dataPartClass, "\" is not eligible to be the data part of another class (must be a basic class or a virtual class with no slots", sep=""))
          if(is(prototype, "classPrototypeDef"))
          {}
          else if(is(prototype, dataPartClass)) {
              if(is(prototype, "list") && length(names(prototype)) > 0)
                  warning("prototype is a list with named elements (could be ambiguous):  better to use function prototype() to avoid trouble.")
          }
          else if(is.list(prototype))
              prototype <- do.call("prototype", prototype)
      }
      ## pnames will be the names explicitly defined in the prototype
      if(is(prototype, "classPrototypeDef")) {
          pnames <- prototype@slots
          prototype <- prototype@object
      }
      else 
          pnames <- allNames(attributes(prototype))
      ## now set the slots not yet in the prototype object.
      ## An important detail is that these are
      ## set using slot<- with check=FALSE (because the slot will not be there already)
      ## what <- is.na(match(slots, pnames))
      what <- seq(along=properties)
      props <- properties[what]
      what <- slots[what]
      for(i in seq(along=what)) {
          propName <- el(what, i)
          if(!identical(propName, ".Data") &&
             is.null(attr(prototype, propName)))
              slot(prototype, propName, FALSE) <- tryNew(el(props, i))
      }
      list(properties = properties, prototype = prototype)
  }

tryNew <-
  ## Tries to generate a new element from this class, but if the attempt fails
  ## (as, e.g., when the class is undefined or virtual) just returns NULL.
  ##
  ## This is inefficient and also not a good idea when actually generating objects,
  ## but is useful in the initial definition of classes.
  function(Class) {
    if(!isClass(Class) || isVirtualClass(Class))
      return(NULL)
    value <- trySilent(new(Class))
    if(is(value, "try-error"))
      NULL
    else
      value
  }

empty.dump <-
  function()
  list()

isClassDef <-
    function(object)
    is(object, "classRepresentation")

showClass <-
  ## print the information about a class definition.  If complete==TRUE, include the
  ## indirect information about extensions.
  function(Class, complete = TRUE, propertiesAreCalled = "Slots") {
    if(isClassDef(Class)) {
      ClassDef <- Class
      Class <- getClassName(ClassDef)
    }
    else if(complete)
      ClassDef <- getClass(Class)
    else
      ClassDef <- getClassDef(Class)
    if(identical(getVirtual(ClassDef), TRUE))
      cat("Virtual Class\n")
    x <- getProperties(ClassDef)
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
          .class1(getPrototype(ClassDef)), "\"\n", sep="")
    ext <- getExtends(ClassDef)
    if(length(ext)>0) {
      cat("\nExtends:\n")
      showExtends(ext)
    }
    ext <- getSubclasses(ClassDef)
    if(length(ext)>0) {
      cat("\nKnown Subclasses:\n")
      showExtends(ext)
    }
  }

showExtends <-
  ## print the elements of the list of extensions.  Also used to print
  ## extensions recorded in the opposite direction, via a subclass list
  function(ext, printTo = stdout()) {
      what <- names(ext)
      how <- character(length(ext))
      for(i in seq(along=ext)) {
          eli <- el(ext, i)
          if(is(eli, "SClassExtension")) {
              if(length(eli@by) > 0)
                  how[i] <- paste("by class", paste("\"", eli@by, "\"", sep="", collapse = ", "))
              else if(identical(eli@dataPart, TRUE))
                  how[i] <- "from data part"
              else
                  how[i] <- "directly"
              if(!eli@simple) {
                  if(is.function(eli@test)) {
                      if(is.function(eli@coerce))
                          how[i] <- paste(how[i], ", with explicit test and coerce", sep="")
                      else
                          how[i] <- paste(how[i], ", with explicit test", sep="")
                  }
                  else if(is.function(eli@coerce))
                      how[i] <- paste(how[i], ", with explicit coerce", sep="")
              }
          }
      }
      if(identical(printTo, FALSE))
          list(what = what, how = how)
      else cat(file = printTo, paste("Class \"", what, "\", ", how, ".\n", sep=""), sep="")
  }



print.classRepresentation <-
  function(x, ...)
  showClass(x, propertiesAreCalled="Slots")

## assign the empty definition of an environment to be used
## to store session-scope metadata.
##
## Relies on the fact that modifications to objects in attached data, other
## than the working data in position 1, are NOT saved in the image of the session.

## This mechanism needs changing to adapt to threads and/or namespaces
## The class table, and the method table currently methods_dispatch.c, should both
## be defined in the "top level environment" and should be cleared at startup (e.g., by
## .First.lib in the methods package).

SessionClassMetaData <- "__ClassMetaData"
assign(SessionClassMetaData, new.env(), envir = environment())

getFromClassMetaData <-
  substitute(function(name) {
    if(exists(name, envir = NAME, inherits=FALSE))
      get(name, env = NAME)
    else
      NULL
  }, list(NAME=as.name(SessionClassMetaData)))
mode(getFromClassMetaData) <- "function"

assignToClassMetaData <-
  substitute(function(name, value)
             assign(name, value, envir = NAME), list(NAME=as.name(SessionClassMetaData)))
mode(assignToClassMetaData) <- "function"

removeFromClassMetaData <-
  substitute(function(name) rm(list=name, envir=NAME), list(NAME=as.name(SessionClassMetaData)))
mode(removeFromClassMetaData) <- "function"


possibleExtends <-
  ## Find the information that says whether class1 extends class2,
  ## directly or indirectly.  This can be either a logical value or
  ## an object containing various functions to test and/or coerce the relationship.
  function(class1, class2)
{
    if(class1 == class2 || identical(class2, "ANY"))
        return(TRUE)
    i <- NA
    if(isClass(class1)) {
        ClassDef <- getClass(class1)
        ext <- getExtends(ClassDef)
        i <- match(class2, names(ext))
    }
    else
        i <- NA
    if(is.na(i)) {
        if(isClass(class2) &&
           !is.na(match(class1, names(getSubclasses(getClass(class2))))))
            TRUE
        else
            FALSE
    }
    else {
         el(ext, i)
    }
}

completeExtends <-
  ## complete the extends information in the class definition, by following
  ## transitive chains.
  ##
  ## Elements in the immediate extends list may be added and current elements may be
  ## replaced, either by replacing a conditional relation with an unconditional
  ## one, or by adding indirect relations.
  ##
    function(ClassDef)
       .completeExtBreadth(ClassDef, "contains")$exts

completeSubclasses <-
    function(ClassDef) {
        value <- .completeExtBreadth(ClassDef, "subclasses")$exts
        superClasses <- names(ClassDef@contains)
        if(length(value) >0 && length(superClasses) > 0) {
            ## append the subclass info to any completed superclasses
            for(Class in superClasses) {
                superDef <- getClassDef(Class, 0)
                if(is(superDef, "classRepresentation")) {
                    superDef@subclasses[names(value)] <- value
                    assignClassDef(Class, superDef, 0)
                }
            }
        }
        value
    }
                    


.completeExtBreadth <-  function(ClassDef, slotName, soFar = getClassName(ClassDef), level = 1)
{
    ext <- slot(ClassDef, slotName)
    from <- getClassName(ClassDef)
    what <- names(ext)
    if(!all(is.na(match(what, soFar)))) {
        ## watch for loops (reflexive is relations, e.g.)
        ok <- is.na(match(what, soFar))
        ext <- ext[ok]
        what <- what[ok]
    }
    value <- list(exts = ext, what = what, level = rep(level, length(ext)))
    soFar <- c(soFar, what)
    for(i in seq(along=ext)) {
        by <- what[[i]]
        if(isClass(by)) {
            byDef <- getClass(by)
            if(!is(byDef, "classRepresentation")) # the temporary def'n:  looks like a loop
                next
            valuei <- .completeExtBreadth(byDef, slotName, soFar, level+1)
            exti <-  valuei$exts
            if(identical(slotName, "contains"))
                ## infer the form of the transitive extensions
                exti <- .transitiveExtends(from, by, ext[[i]], exti, getSlots(ClassDef))
            value$exts <- c(value$exts, exti)
            value$what <- c(value$what, valuei$what)
            value$level <- c(value$level, valuei$level)
        }
    }
    ## look for duplicate entries, resolve by level
    allWhat <- unique(value$what)
    if(length(allWhat) < length(value$what)) {
        what <- value$what
        levels <- value$level
        dups <- unique(what[duplicated(what)])
        n <- length(what)
        keep <- rep(TRUE, n)
        ## select one of the duplicates with the minimal inheritance
        ## level, for each duplicated value.  Keep this, drop the rest
        for(el in dups) {
            ii <- what == el
            lmin <- min(levels[ii])
            pick <- (1:n)[ii & levels == lmin][1]
            keep[ii] <- FALSE
            keep[pick] <- TRUE
        }
        value$exts <- value$exts[keep]
        value$what <- value$what[keep]
        value$level <- value$level[keep]
    }
    value
}


classMetaName <-
  ## a name for the object storing this class's definition
  function(name)
  methodsPackageMetaName("C", name)

extendsMetaName <-
    function(ClassDef)
    methodsPackageMetaName("EXT", if(missing(ClassDef)) ""
                           else paste(getClassName(ClassDef),
                                      getClassPackage(ClassDef), sep=":"))

subclassesMetaName <-
    function(ClassDef)
    methodsPackageMetaName("SUB", if(missing(ClassDef)) ""
                           else paste(getClassName(ClassDef),
                                      getClassPackage(ClassDef), sep=":"))

methodsPackageMetaName <-
  ## a name mangling device to simulate the meta-data in S4
  function(prefix, name)
  paste(".", prefix, name, sep="__")



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
           message = paste("No method defined for signature",
           paste(signature, collapse=", ")))
{
    for(f in functions) {
        method <- getMethod(f, optional = TRUE)
        if(!is.function(method))
            method <- getGeneric(f)
        body(method) <- substitute(stop(MESSAGE), list(MESSAGE=message))
        environment(method) <- .GlobalEnv
        setMethod(f, signature, method)
    }
}

getSlots <- function(x, complete = TRUE) {
    if(isClassDef(x))
        classDef <- x
    else
        classDef <- (if(complete) getClass(x) else getClassDef(x))
    props <- getProperties(classDef)
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

getDataPart <- function(object) {
    temp <- getSlots(class(object))
    slots <- names(temp)
    iData <- match(".Data", slots)
    if(is.na(iData))
        stop(paste("class \"", class(object),
                   "\" does not have a data part (a .Data slot) defined",
                   sep=""))
    dataClass <- temp[[iData]]
    slots <- slots[-iData]
    for(what in slots)
        attr(object, what) <- NULL
    ## data part classes must either be a basic class or a virtual class
    ## with no slots (which is then supposed to be extended by basic classes)
    ## setting the class to NULL in the S4 interpretation makes the object belong
    ## to the appropriate basic class.
    class(object) <- NULL
    object
}

setDataPart <- function(object, value) {
    dataClass <- elNamed(getSlots(class(object)), ".Data")
    if(is.null(dataClass))
        stop(paste("class \"", class(object),
                   "\" does not have a data part (a .Data slot) defined",
                   sep=""))
    value <- as(value, dataClass)
    .mergeAttrs(value, object)
}

.validDataPartClass <- function(cl) {
    ClassDef <- getClass(cl, TRUE)
    value <- elNamed(ClassDef@slots, ".Data")
    if(is.null(value)) {
        if(identical(cl, "structure"))
            value <- "vector"
        else if((extends(cl, "vector") || !is.na(match(cl, .BasicClasses))))
            value <- cl
        else {
            if(identical(ClassDef@virtual, TRUE) &&
               length(getProperties(ClassDef)) == 0 &&
               length(ClassDef@subclasses) > 0 &&
               all(!is.na(match(names(ClassDef@subclasses), .BasicClasses))))
                value <- cl
        }
    }
    value
}

.mergeAttrs <- function(value, object, explicit = NULL) {
    supplied <- attributes(object)
    if(length(explicit)>0)
        supplied[names(explicit)] <- explicit
    valueAttrs <- attributes(value)
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
.transitiveExtends <- function(from, by, byExt, moreExts, fromSlots) {
    what <- names(moreExts)
    for(i in seq(along = moreExts)) {
        toExt <- moreExts[[i]]
        to <- what[[i]]
        toExt@by <- by
        ## construct the composite coerce method, taking into account the strict=
        ## argument.
        f <- toExt@coerce
        fR <- toExt@replace
        ## if both are simple extensions, so is the composition
        if(byExt@simple && toExt@simple) {
            toExpr <- body(f)
            fBy <- byExt@coerce
            byExpr <- body(fBy)
            expr <- (if(byExt@dataPart)
                     substitute({from <- from@.Data; EXPR},
                                list(EXPR = toExpr))
                   else if(toExt@dataPart)
                     substitute({from <- EXPR;  from@.Data},
                                list(EXPR = byExpr))
                   else  (if(identical(byExpr, quote(from)) && identical(toExpr, quote(from)))
                           quote(from)
                         else
                           substitute({from <- E1; E2}, list(E1 = byExpr, E2 = toExpr))
                         )
                     )
            body(f, envir = environment(f)) <- expr
        }
        else if(byExt@simple) {
            body(f, envir = environment(f)) <-
                substitute( as(if(strict) as(from, BY) else from, TO, strict=strict),
                           list(BY = by, TO = to))
        }
        else if(toExt@simple) {
            ## in this case, coercing to `by' and testing for `by' are
            ## sufficient, since the further step to the target is a simple extension.
            body(f, envir = environment(f)) <-
                substitute({ from <- as(from, BY, strict = strict);
                             if(strict) as(from, TO) else from},
                           list(BY = by, TO = to))
            toExt@test <- byExt@test
        }
        else {
            ## we could improve the efficiency by checking for special cases
            ## (e.g., simple test's).  But a composite of two non-simple extensions
            ## seems pretty weird.  So we'll wait for examples to show up.
            body(f, envir = environment(f)) <-
                substitute(as(as(from, BY), TO),
                           list(BY = by, TO = to))
            ff <- byExt@test
            body(ff, envir = environment(ff)) <-
                substitute((is(object, BY) && is(as(object, BY), TO)),
                          list(BY = by, TO = to)) 
            toExt@test <- ff
        }
        toExt@coerce <- f
        f <- byExt@replace
        expr <- body(f)
        expr <- .insertExpr(expr,
                            substitute(value <- as(value, TO), list(TO=to)))
        body(f, envir = environment(f)) <- expr
        toExt@replace <- f
        moreExts[[i]] <- toExt
    }
    moreExts
}

## construct the expression that copies slots into the new object
## The fromSlots argument is provided for calls from makeClassRepresentation
## and completeClassDefinition,
## when the fromClass is in the process of being defined, so slotNames() would fail
.simpleCoerceExpr <- function(fromClass, toClass, fromSlots = slotNames(fromClass)) {
    toSlots <-  slotNames(toClass)
    sameSlots <- (length(toSlots) == 0
                  || (length(fromSlots) == length(toSlots) &&
                      !any(is.na(match(fromSlots, toSlots)))))
    if(sameSlots)
        substitute({class(from) <- CLASS; from},
                   list(CLASS = toClass))
    else
        substitute({
            value <- new(CLASS)
            for(what in TOSLOTS)
                slot(value, what) <- slot(from, what)
            value }, list(CLASS=toClass, TOSLOTS = toSlots))
}

## the boot version of newClassRepresentation (does no checking on slots to avoid
## requiring method selection on coerce).

newClassRepresentation <- function(...) {
    value <- new("classRepresentation")
    slots <- list(...)
    slotNames <- names(slots)
    for(i in seq(along = slotNames))
        slot(value, slotNames[[i]], FALSE) <- slots[[i]]
    value
}

## create a temporary definition of a class, but one that is distinguishable
## (by its class) from the real thing.  See comleteClassDefinition
.tempClassDef <- function(...) {
    value <- list()
    slots <- list(...)
    slotNames <- names(slots)
    for(i in seq(along = slotNames))
        slot(value, slotNames[[i]], FALSE) <- slots[[i]]
    value
}
    
## the real version of newClassRepresentation, assigned in .First.lib
.newClassRepresentation <- function(...)
    new("classRepresentation", ...)

.removeSubclassLinks <- function(Class, package, subclasses) {
    for(subclass in names(subclasses)) {
        subclassDef <- getClassDef(subclass)
        if(!is(subclassDef, "classRepresentation"))
            next
        what <- extendsMetaName(subclassDef)
        where <- find(what)
        for(pos in where) {
            obj <- get(what, pos)
            if(!is.null(elNamed(obj, Class))) {
                elNamed(obj, Class) <- NULL
                if(length(obj) > 0)
                    assign(what, obj, pos)
                else
                    rm(list=what, pos = pos)
                resetClass(subclass)
            }
        }
    }
}

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
.class1 <- function(x)
    class(x)[[1]]

substituteFunctionArgs <- function(def, newArgs, args = formalArgs(def), silent = FALSE) {
    if(!identical(args, newArgs)) {
        n <- length(args)
        if(n != length(newArgs))
            stop(paste("Trying to change the argument list of a function with ",
                       n, " arguments to have arguments (",
                 paste(newArgs, collapse = ", "), ")", sep=""))
        bdy <- body(def)
        ## check for other uses of newArgs
        checkFor <- newArgs[is.na(match(newArgs, args))]
        locals <- all.vars(bdy)
        if(length(checkFor) > 0 && any(!is.na(match(checkFor, locals))))
            stop(paste("Get rid of variables in definition (",
                       paste(checkFor[!is.na(match(checkFor, locals))], collapse = ", "),
                       "); they conflict with the needed change to argument names (",
                       paste(newArgs, collapse = ", "), ")", sep=""))
        ll <- vector("list", 2*n)
        for(i in seq(length = n)) {
            ll[[i]] <- as.name(args[[i]])
            ll[[n+i]] <- as.name(newArgs[[i]])
        }
        names(ll) <- c(args, newArgs)
        body(def, envir = environment(def)) <- substituteDirect(bdy, ll)
        if(!silent)
            message("Arguments in definition changed from (",
                    paste(args, collapse = ", "), ") to (",
                    paste(newArgs, collapse = ", "), ")")
    }
    def
}    

.makeValidityMethod <- function(Class, validity) {
    if(is.null(validity)) {
    }
    else {
        if(!is(validity, "function"))
            stop(paste("A validity method must be a function of one argument, got an object of class \"",
                       class(validity), "\"", sep=""))
        validity <- substituteFunctionArgs(validity, "object")
    }
    validity
}

# the bootstrap version of setting slots in completeClassDefinition
.mergeClassDefSlots <- function(ClassDef, ...) {
    slots <- list(...); slotNames <- names(slots)
    for(i in seq(along = slots))
        slot(ClassDef, slotNames[[i]], FALSE) <- slots[[i]]
    ClassDef
}

## the real version:  differs only in checking the slot values
..mergeClassDefSlots <- function(ClassDef, ...) {
    slots <- list(...); slotNames <- names(slots)
    for(i in seq(along = slots))
        slot(ClassDef, slotNames[[i]]) <- slots[[i]]
    ClassDef
}
