
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
            if(is.null(prototype))
                prototype <- getPrototype(getClass(what))
            else {
                ## two super classes => prototype not defined?
                warning("More than one non-virtual superclass: prototype may be ambiguous")
                break
            }
        }
    }
    if(length(properties) == 0)
        return(prototype)
    if(is.null(prototype))
        prototype <- defaultPrototype()
    pnames <- names(attributes(prototype))
    snames <- names(properties)
    for(j in seq(along = properties)) {
        name <- el(snames, j)
        i <- match(name, pnames)
        if(is.na(i))
            slot(prototype, name, check=FALSE) <- tryNew(el(properties, j))
    }
    pnames <- pnames[is.na(match(pnames, snames))]
    if(length(pnames)>0)
        warning(paste("Slots in prototype and not in class:",
                      paste(pnames, collapse=", ")))
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
        environment(ClassDef) <- copyEnvironment(ClassDef)
        ## copy the environment so the completion will not be saved beyond the
        ## session.
        assignClassDef(Class, ClassDef, 0)
        ## an initial assignment prevents recursive looping should this class's
        ## definition be needed during the computations (such loops are usually but
        ## not quite always an error).  Removed on exit: completeClassDefinition
        ## does not store the definition.  It is called by getClass, which does, and
        ## by setSClass, which does not.
        on.exit(removeClass(Class, 0))
        ev <- environment(ClassDef)
        properties <- getProperties(ClassDef)
        immediate <- getExtends(ClassDef)
        ext <- getAllSuperClasses(ClassDef)
        ## all the direct and indirect superClasses but NOT those that do
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
            if(any(duplicated(names(properties)))) {
                duped <- duplicated(names(properties))
                dupNames <- names(properties)[duped]
                dupClasses <- logical(length(superProps))
                for(i in seq(along = superProps)) {
                    dupClasses[i] <- !all(is.na(match(dupNames, names(superProps[[i]]))))
                }
                stop(paste("Duplicate slot names: slots ",
                        paste(dupNames, collapse =", "), "; see classes ",
                        paste(c(Class, ext)[dupClasses], collapse = ", "), sep=""))
                properties <- properties[!duped]
            }
        }
        prototype <- makePrototypeFromClassDef(properties, getPrototype(ClassDef), immediate)
        virtual <- getVirtual(ClassDef)
        validity <- getValidity(ClassDef)
        access <- getAccess(ClassDef)
        if(is.na(virtual))
            ## compute it from the immediate extensions, but all the properties
            virtual <- testVirtual(properties, immediate, prototype)
        newEv <- newClassEnvironment(Class, properties,
                                     completeExtends(ClassDef),
                                     prototype,
                                     getSubclasses(ClassDef),
                                     virtual,
                                     validity,
                                     access)
        environment(ClassDef) <- newEv
    }
    else {
        ## create a class definition of an empty virtual class
        ClassDef <- getClass("VIRTUAL")
        environment(ClassDef) <- copyEnvironment(environment(ClassDef))
        setClassName(ClassDef, Class)
    }
    ClassDef
}

getFromClassDef <-
  ## Extracts one of the intrinsically defined class definition properties
  ## (".Poperties", etc.)  Strictly a utility function
  function(ClassDef, what)
    get(what, envir = environment(ClassDef))

setInClassDef <-
  ## Set Property in Class Definition
  ## set one of the intrinsically defined class definition properties
  ## (".Poperties", etc.)  Strictly a utility function
  function(ClassDef, what, value, synchronize = TRUE)
{
    assign(what, value, envir = environment(ClassDef))
    assign(".Synchronized", FALSE, envir = environment(ClassDef))
    if(synchronize)
        synchronizeClassDef(ClassDef)
    what
}

synchronizeClassDef <-
  ## Does whatever is needed to synchronize information in the class definition.
  ##
  ## Basically computes derived information used to make object manipulations more efficient
  ## but that need to be revised if information changes.
  ##
  ## (Nothing at the moment)
  function(ClassDef) {
  }

getProperties <-
  ## Extracts the class's Properties information from the class representation (only, not from
  ## the name of the class).
  function(ClassDef) {
   getFromClassDef(ClassDef, ".Properties")
 }

setProperties <-
  ## Sets the class's Properties information given the class representation (only, not from
  ## the name of the class)
  function(ClassDef, value)
  setInClassDef(ClassDef, ".Properties", value)

getExtends <-
  ## extract the class's Extends information from the class representation (only, not from
  ## the name of the class)
  ##
  ## Contrast with the `findExtends' and `is' functions, both of which use indirect
  ## information as well.
  function(ClassDef)
    getFromClassDef(ClassDef, ".Extends")

getValidity <-
   ## extract the class's Validity method (or NULL) from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
    getFromClassDef(ClassDef, ".Validity")
 

getAccess <-
   ## extract the class's Access method (or NULL) from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
    getFromClassDef(ClassDef, ".Access")
 

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
  function(ClassDef, soFar = getClassName(ClassDef) )
{
    ext <- getExtends(ClassDef)
    ## remove superclasses defined with a coerce method.  We can't use these
    ## to infer information about slots, etc.
    ok <- rep(TRUE, length(ext))
    for(i in seq(along=ext)) {
        exti <- ext[[i]]
        if(is.list(exti) && is.function(exti$coerce))
            ok[i] <- FALSE
    }
    ext <- ext[ok]
    immediate <- names(ext)
    ## watch out for loops (e.g., matrix/array have mutual is relationship)
    immediate <- immediate[is.na(match(immediate, soFar))]
    soFar <- c(soFar, immediate)
    super <- list(label=immediate, depth = rep(1, length(immediate)))
    for(what in immediate) {
        if(isClass(what)) {
            more <- Recall(getClassDef(what), soFar)
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

setExtends <-
  ## set the class's Extends information given the class representation (only, not from
  ## the name of the class)
  function(ClassDef, value)
    setInClassDef(ClassDef, ".Extends", value)

getPrototype <-
  ## extract the class's Prototype information from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
  getFromClassDef(ClassDef, ".Prototype")

setPrototype <-
  ## set the class's Prototype information given the class representation (only, not from
  ## the name of the class)
  function(ClassDef, value)
  setInClassDef(ClassDef, ".Prototype", value)

getVirtual <-
  ## extract the class's Virtual information from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
  getFromClassDef(ClassDef, ".Virtual")

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

setVirtual <-
  ## set the class's Virtual information given the class representation (only, not from
  ## the name of the class)
  function(ClassDef, value)
  setInClassDef(ClassDef, ".Virtual", value)

getSubclasses <-
  ## extract the class's Subclasses information from the class representation (only, not from
  ## the name of the class)
  function(ClassDef)
  getFromClassDef(ClassDef, ".Subclasses")

setSubclasses <-
  ## set the class's Subclasses information given the class representation (only, not from
  ## the name of the class)
  function(ClassDef, value)
  setInClassDef(ClassDef, ".Subclasses", value)

getClassName <-
  ## The internal property in the class definition for the class name.
  function(ClassDef)
  getFromClassDef(ClassDef, ".ClassName")

setClassName <-
  ## set the name of the class inside the class definition
  function(ClassDef, value)
  setInClassDef(ClassDef, ".ClassName", value)


assignClassDef <-
  ## assign the definition of the class to the specially named object
  function(Class, def, where = .GlobalEnv) {
    if(identical(where, 0))
      assignToClassMetaData(classMetaName(Class), def)
    else assign(classMetaName(Class), def, where)
  }

newClassEnvironment <-
  function(name, properties, extends, prototype, subclasses, virtual, validity, access) {
    ev <- new.env()
    assign(".ClassName", name, ev)
    assign(".Extends", extends, ev)
    assign(".Properties", properties, ev)
    assign(".Prototype", prototype, ev)
    assign(".Subclasses", subclasses, ev)
    assign(".Virtual", virtual, ev)
    assign(".Validity", validity, ev)
    assign(".Access", access, ev)
    return(ev)
  }


newBasic <-
  ## the implementation of the function `new' for basic classes that don't have
  ## a formal definition.  Any of these could have a formal definition, except for
  ## Class="NULL" (disallowed because NULL can't have attributes).
  ##
  ## See `new' for the interpretation of the arguments.
  function(Class, ..., .Force = FALSE) {
  value <- switch(Class,
               "NULL" = return(NULL), ## can't set attr's of NULL in R
               "logical" =,
               "numeric" =,
               "character" =,
               "complex" =,
               "integer" =,
               "double" =,
                "expression" =,
               "list" =  as.vector(c(...), Class),
               "single" =, as.single(c(...)),
              "environment" = new.env(),
               "function" = quote(function()NULL),
               "named" = named(...),
                  ## note on array, matrix:  not possible to be compatible with
                  ## S-Plus on array, unless R allows 0-length .Dim attribute
               "array" = (if(length(list(...)) > 0) array(...) else structure(numeric(), .Dim =0)),
               "matrix" = (if(length(list(...)) > 0) matrix(...) else matrix(0,0,0)),
               "ts" = ts(...),
            ## The language data
                  "name" = as.name("<UNDEFINED>"), # R won't allow 0 length names
                  "call" = quote({}), ## general expressions all get data.class=="call"
                  {
                      if(.Force)
                          ## create an empty object, even though this class is undefined
                          ## for why this is not NULL, see the documentation for `new'.
                          return(newEmptyObject())
                      else if(is.na(Class, .BasicClasses))
                          stop(paste("Calling new() on an undefined and non-basic class (\"",
                               Class, "\")", sep=""))
                      else
                          stop(paste("Basic class \"", Class, "\" cannot be instantiated by calling new()",
                                     sep =""))
                  }
                  )
  value
}

makeExtends <-
  ## convert the argument to a list defining the extension mechanism.
  function(extends) {
    if(is.character(extends)) {
      value <- rep(list(TRUE), length(extends))
      names(value) <- extends
      value
    }
    else if(is.list(extends)) {
      cnames <- allNames(extends)
      for(i in which(nchar(cnames)==0)) {
        cl <- el(extends, i)
        if(is.character(cl) && length(cl) == 1) {
          el(cnames, i) <- cl
          el(extends, i) <- TRUE
        }
        else
          stop(paste("unnamed extension element (", i, ") must be a string", sep=""))
      }
      names(extends) <- cnames
      extends
    }
    else stop(paste("extends argument must be a list or a vector of class names", sep=""))
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
  function(name, properties, prototype, extends) {
      ## the StandardPrototype should really be a type that doesn't behave like
      ## a vector.  But none of the existing types work.  Someday ...
      StandardPrototype <- defaultPrototype()
      superClasses <- names(extends)
      undefined <- rep(FALSE, length(superClasses))
      slots <-  allNames(properties)
      allSlots <- character()
      for(i in seq(along=superClasses)) {
          cl <- superClasses[[i]]
          if(isClass(cl))
              allSlots <- c(allSlots, slotNames(cl))
          else
              undefined[[i]] <- TRUE
      }
      if(any(undefined)) {
          warning("Prototype may be incomplete, not all superclasses defined (",
                  paste(superClasses[undefined], collapse = ", "), ")")
      }
      undefined <- any(undefined) ## used as  a test later.
      ## check for conflicts in the slot names
      if(any(duplicated(allSlots))) {
          warning("The inherited slots contain duplicates (",
                  paste(allSlots[duplicated(allSlots)], collapse = ", "),
                  ")")
          allSlots <- unique(allSlots)
      }
      if(any(!is.na(match(slots, allSlots)))) {
          ## representation catches this already, but for completeness:
          warning("Class \"", name, "\" overrides some inherited slot definitions (",
                  paste(allSlots[!is.na(match(slots, allSlots))], collapse = ", "),
                  ")")
          allSlots <- unique(slots, allSlots)
      }
      ## A rule is needed to decide whether the environment should be hashed.
      ## decide on a prototype, if one was not provided
      if(is.null(prototype) && is.na(match("VIRTUAL", superClasses))) {
          basicSuperClasses <- .BasicClasses[!is.na(match(superClasses, .BasicClasses))]
          if(length(basicSuperClasses) > 0) {
              if(length(basicSuperClasses) > 1) {
                  warning("Class \"",name,"\" extends more than one basic class (",
                          paste(basicSuperClasses, collapse = ", "), "), using ",
                          basicSuperClasses[1])
                  basicSuperClasses <- basicSuperClasses[1]
              }
              prototype <- newBasic(basicSuperClasses)
          }
          else if(any(!is.na(match(superClasses, c("vector", "structure")))))
              prototype <- newBasic("logical")
          if(is.null(prototype)) { ## non-vector (may extend NULL)
              prototype <- StandardPrototype
          }
      }
      else if(is.list(prototype) && length(names(prototype)) > 0) {
          ## the prototype as a named list is allowed, mostly for S-Plus compatibility
          pnames <- names(prototype)
          realP <- StandardPrototype
          for(i in seq(along=pnames))
              slot(realP, pnames[[i]], FALSE) <- elNamed(prototype, pnames[i])
          prototype <- realP
      }
      if(is.null(prototype))
          prototype <- StandardPrototype
      ## pnames will be the names explicitly defined in the prototype
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
          if(is.null(attr(prototype, propName))) {
              slot(prototype, propName, FALSE) <- tryNew(el(props, i))
          }
      }
      prototype
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
    opt1 <- options(show.error.messages = FALSE)
    opt2 <- options(error = quote(empty.dump()))
    ## following is a workaround of a bug in options that does not
    ## acknowledge NULL as an options value => delete this element.
    if(is.null(opt1[[1]]))
      on.exit({options(show.error.messages = TRUE); options(opt2)})
    else
      on.exit({options(opt1); options(opt2)})
    value <- try(new(Class))
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
    is(object, "classRepEnvironment")

showClass <-
  ## print the information about a class definition.  If complete==TRUE, include the
  ## indirect information about extensions.
  function(Class, complete = TRUE, printTo = stdout(), propertiesAreCalled = "Properties") {
      if(identical(printTo, FALSE)) {
          tmp <- tempfile()
          con <- file(tmp, "w")
      }
      else
          con <- printTo
    if(isClassDef(Class)) {
      ClassDef <- Class
      Class <- getClassName(ClassDef)
    }
    else if(complete)
      ClassDef <- getClass(Class)
    else
      ClassDef <- getClassDef(Class)
    if(identical(getVirtual(ClassDef), TRUE))
      cat(file = con, "Virtual Class\n")
    x <- getProperties(ClassDef)
    if(length(x)>0) {
        n <- length(x)
        cat(file = con, "\n",propertiesAreCalled, ":\n", sep="")
        text <- format(c(names(x), as.character(x)), justify="right")
        cat(file = con, paste(c("Name: ", text[seq(length=n)]), collapse=" "), "\n")
        cat(file = con, paste(c("Class:", text[seq(n+1, length=n)]), collapse=" "), "\n")
    }
    else
      cat(file = con, "\nNo ", propertiesAreCalled, ", prototype of class \"",
          data.class(getPrototype(ClassDef)), "\"\n", sep="")
    ext <- getExtends(ClassDef)
    if(length(ext)>0) {
      cat(file = con, "\nExtends:\n")
      showExtends(ext, con)
    }
    ext <- getSubclasses(ClassDef)
    if(length(ext)>0) {
      cat(file = con, "\nKnown Subclasses:\n")
      showExtends(ext, con)
    }
    if(identical(printTo, FALSE)) {
        close(con)
        value <- readLines(tmp)
        unlink(tmp)
        value
    }
  }

showExtends <-
  ## print the elements of the list of extensions.  Also used to print
  ## extensions recorded in the opposite direction, via a subclass list
  function(ext, printTo = stdout()) {
      if(identical(printTo, FALSE)) {
          tmp <- tempfile()
          con <- file(tmp, "w")
      }
      else
          con <- printTo
    what <- names(ext)
    for(i in seq(along=ext)) {
      cat(file = con, "Class \"", el(what, i), "\" ", sep="")
      eli <- el(ext, i)
      if(length(eli$by) > 0)
        how <- paste("by class", paste("\"", eli$by, "\"", sep="", collapse = ", "))
      else
        how <- "directly"
      if(is.function(eli$test)) {
        if(is.function(eli$coerce))
          how <- paste(how, ", with explicit test and coerce", sep="")
        else
          how <- paste(how, ", with explicit test", sep="")
      }
      else if(is.function(eli$coerce))
        how <- paste(how, ", with explicit coerce", sep="")
      cat(file = con, how, ".\n", sep="")
    }
    if(identical(printTo, FALSE)) {
        close(con)
        value <- readLines(tmp)
        unlink(tmp)
        value
    }
  }



print.classRepEnvironment <-
  function(x, ...)
  showClass(x, prop="Slots")

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


extendsCoerce <-
  ## the function to perform coercion based on the is relation
  ## between two classes.  May be explicitly stored in the metadata or
  ## inferred.  If the latter, a function is constructed to do the coerce.
    ## The function is returned (to `as', which will cache it as a coerce method).
    ##
    ## When called (from method selection) with formFunction=FALSE, the point is
    ## to determine whether there are tests and/or coerce functions that need to be
    ## applied in forming the method to be cached.
  function(fromClass, Class, formFunction = TRUE)
{
    ext <- findExtends(fromClass, Class)
    f <- NULL
    if(is.list(ext) && is.null(ext$by)) {
        ## an explicit function exists (maybe constructed on a
        ## previous call):  return it now.
        coe <- ext$coerce
        if(is.function(coe)) {
            f <- function(from) NULL
            oldArg <- formalArgs(coe)[1]
            sublist <- list(quote(from), as.name(oldArg))
            names(sublist) <- c(oldArg, "from")
            body(f) <- substituteDirect(body(coe), sublist)
        }
        test <- ext$test
        if(is.function(test)) {
            ## incorporate a test
            if(is.null(f))
               f <- function(from, to)f
            body(f) <- substitute({if(!is(from, CLASS))
                                  stop(paste('Conditional inheritance from', CLASS, 'not valid'));
                              DEF}, list(DEF = body(f), CLASS = Class))
        }
    }
    else if(is.character(ext) || !is.null(ext$by)) {
        ## TO DO:  this does not incorporate the case of a conditional extension
        by <- (if(is.character(ext)) ext else ext$by)
        if(formFunction) {
            f <- eval(substitute(function(from)
                            as(as(from, BY), CLASS),
                            list(BY = by, CLASS=Class)))
        }
        else
            ## call recursively, to determine if the relation
            ## is eventually explicit (f will be a function) or not
            f <- Recall(by, Class, formFunction)
    }
    if(is.null(f)) {
        if(!formFunction)
            return(TRUE)
        ## Because `is' was TRUE, must be a direct extension.
        ## Copy slots if the slots are a subset.  Else, just set the
        ## class.  For VIRTUAL targets, never change the object.
        ## If the to Class is not formally defined, the `is' is taken to imply
        ## that the object's contents are a Class object.
        formal <- isClass(Class)
        virtual <- formal && isVirtualClass(Class)
        if(!formal)
            f <- function(object)unclass(object)
        else if(virtual)
            f <- function(object)object
        else {
            fromSlots <- slotNames(fromClass)
            toSlots <-  slotNames(Class)
            sameSlots <- (length(toSlots) == 0
                          || (length(fromSlots) == length(toSlots) &&
                              !any(is.na(match(fromSlots, toSlots)))))
            if(sameSlots)
                f <- substitute(function(from){class(from) <- CLASS; from},
                                list(CLASS = Class))
            else
                f <- substitute(function(from) {
                    value <- new(CLASS)
                    for(what in TOSLOTS)
                        slot(value, what) <- slot(from, what)
                    value }, list(CLASS=Class, TOSLOTS = toSlots))
            ## bug in R: substitute of a function gives a call
            f <- eval(f)
        }
    }
    if(is.function(f))
        environment(f) <- .GlobalEnv
    f
}

extendsReplace <-
  ## the function to perform as() replacement based on the is relation
  ## between two classes.  May be explicitly stored in the metadata or
  ## inferred.  The result is converted into a function suitable as a method
    ## for "coerce<-"
  function(fromClass, Class)
{
    ext <- findExtends(fromClass, Class)
    f <- NULL
    if(is.list(ext)) {
        repl <- ext$replace
        if(is.function(repl)) {
            f <- function(from, value) NULL
            body(f, envir = .GlobalEnv) <- substituteDirect(body(repl),
                                        list(object = quote(from), from = quote(.from)))
        }
    }
    else if(is.character(ext)) {
        by <- ext
        if(length(by) > 0) {
          f <- eval(substitute(function(from, value)
                          as(as(from, BY), CLASS) <- value,
                          list(BY = by, CLASS=Class)))
          environment(f) <- .GlobalEnv
        }
        ## else, drop through
    }
    if(is.null(f)) {
        ## Because `is' was TRUE, must be a direct extension.
        ## Copy slots if the slots are a subset.  Else, just set the
        ## class.  For VIRTUAL targets, never change the object.
        ## If the to Class is not formally defined, the `is' is taken to imply
        ## that the object's contents are a Class object.
        formal <- isClass(Class)
        virtual <- formal && isVirtualClass(Class)
        if(!formal || virtual)
            f <- NULL
         else {
            fromSlots <- slotNames(fromClass)
            toSlots <-  slotNames(Class)
            sameSlots <- (length(toSlots) == 0
                          || (length(fromSlots) == length(toSlots) &&
                              !any(is.na(match(fromSlots, toSlots)))))
            if(sameSlots)
                f <- substitute(function(object, value){as(value, CLASS)},
                                list(CLASS = Class))
            else
                f <- substitute(function(from, to, value) {
                     for(what in TOSLOTS)
                        slot(from, what) <- slot(value, what)
                    from}, list(CLASS=Class, TOSLOTS = toSlots))
            f <- eval(f)
            environment(f) <- .GlobalEnv
        }
    }
    f
}

findExtends <-
  ## Find the information that says whether class1 extends class2,
  ## directly or indirectly.  This can be either a logical value or
  ## an object containing various functions to test and/or coerce the relationship.
  function(class1, class2)
{
    if(class1 == class2)
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
  ## The resulting extends list is presented in depth-first order; that is, the
  ## first immediate superclass followed by all the indirect relations through it,
  ## then the next immediate superclass, etc.  Depth first order is required for
  ## consistent elaboration of inherited methods during dispatch, because the
  ## method dispatcher stores the inherited method under the immediate class name.
  ## Under rather obscure situations of multiple inheritance, the result could be
  ## ambiguous (depending on the order in which signatures are seen by the dispatcher
  ## for a particular generic function), unless searching is done depth first.
  function(ClassDef, soFar = getClassName(ClassDef))
{
    ext <- getExtends(ClassDef)
    what <- names(ext)
    if(!all(is.na(match(what, soFar)))) {
        ## watch for loops (reflexive is relations, e.g.)
        ok <- is.na(match(what, soFar))
        ext <- ext[ok]
        what <- what[ok]
    }
    test <- sapply(ext, function(obj)is.list(obj) && is.function(obj$test))
    value <- list()
    for(i in seq(along=ext)) {
        by <- el(what, i)
        valueEl <- ext[i]               ## note: an extra level of list, to be unlisted later
        if(isClass(by))
        {
            more <- Recall(getClass(by), c(soFar, what))
            whatMore <- names(more)
            for(j in seq(along=more)) {
                cl <- el(whatMore, j)
                clTo <- el(more, j)
                ii <- match(cl, what)
                if(is.na(ii)) {
                    ## append to this element of the value (to elaborate the extends
                    ## list in depth-first order).
                    what <- c(what, cl)
                    elNew <- list(by=by)
                    if(el(test,i) || !is.null(clTo$test))
                        elNew$test <- TRUE
                    elNamed(valueEl, cl) <- elNew
                }
                else {
                    if(!el(test,i))
                        next            ## path already there with no test
                    elNew <- el(ext, i)
                    if(!clTo$test) {
                        ## becomes an indirect, but no test
                        elNew$test <- NULL
                        elNew$by <- by  ## only this path needed
                    }
                    else {
                        ## accumulate the vector of possible intermediates
                        ## (initially NULL)
                        elNew$by <- c(elNew$by, by)
                    }
                    el(valueEl, 1) <- elNew
                }
            }
        }
        el(value, i) <- valueEl
    }
    unlist(value, recursive=FALSE)
}


classMetaName <-
  ## a name for the object storing this class's definition
  function(name)
  methodsMetaName("C", name)

methodsMetaName <-
  ## a name mangling device to simulate the meta-data in S4
  function(prefix, name)
  paste(".", prefix, name, sep="__")



as.data.frame <-
function (x, row.names = NULL, optional = FALSE)
{
    if (is.null(x))
        return(as.data.frame(list()))
    if (is.null(attr(x, "class")))
        attr(x, "class") <- data.class(x)
    UseMethod("as.data.frame", x, row.names, optional)
}

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
    value <- as(props, "character")
    names(value) <- names(props)
    value
}

print.environment <-
    function(x, ...) {
        if(is.null(attr(x, "class")))
            print.default(x, ...)
        else
            showNonVector(x, ...)
    }

showNonVector <-
    function(object, printTo = stdout()) {
        if(identical(printTo, FALSE)) {
            tmp <- tempfile()
            con <- file(tmp, "w")
        }
        else
            con <- printTo
        cat(file = con, "Object of class:", class(object))
        for(what in objects( object)) {
            cat(file = con, "\nSlot \"", what, "\":\n")
            show(get(what, object), con)
        }
        if(identical(printTo, FALSE)) {
            close(con)
            value <- readLines(tmp)
            unlink(tmp)
            value
        }
    }
