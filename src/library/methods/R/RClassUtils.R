"data.class<-" <-
  .Primitive("class<-")


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
        ## we assume the superclass may not be defined yet or may be a basic class, in which case
        ## the testing class has to declare itself VIRTUAL explicitly.
        for(what in en)
            if(!isClass(what) || !isVirtualClass(what))
                return(FALSE)
    }
    (length(properties)==0 && is.null(prototype))
}

makePrototypeFromClassDef <-
  ## Makes the prototype implied by
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
    if(length(properties) == 0) {
        if(!is.null(prototype))
            return(prototype)
        ## try for a single superclass that is not virtual
        supers <- names(extends)
        for(i in seq(along=extends)) {
            if(!is.logical(el(extends, i)))
                next
            what <- el(supers, i)
            if(isClass(what) && !isVirtualClass(what)) {
                if(is.null(prototype))
                    prototype <- getPrototype(getClass(what))
                else {
                    ## two super classes => prototype not defined?
                    prototype <- NULL
                    break
                }
            }
        }
        return(prototype)
    }
    ## make the prototype into a named list if it is
    ## currently a structure with attributes.
    pattrs <- attributes(prototype)
    if(length(pattrs)>0) {
        pattrs$class <- NULL
        pattrs$names <- NULL
    }
    if(length(pattrs) > 0 || length(names(prototype)) == 0)
        prototype <- pattrs
    pnames <- names(prototype)
    snames <- names(properties)
    value <- newEmptyObject()
    for(j in seq(along = properties)) {
        name <- el(snames, j)
        el(snames, j) <- ""             ## for check later
        i <- match(name, pnames)
        if(is.na(i))
            slot(value, name, check=FALSE) <- tryNew(el(properties, j))
        else
            slot(value, name, check=FALSE) <- el(prototype, i)
    }
    snames <- snames[nchar(snames)>0]
    if(length(snames)>0)
        warning(paste("Slots ignored in prototype and not in class:",
                      paste(snames, collapse=", ")))
    value
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
  function(Class)
{
    value <- NULL
    if(isClass(Class)) {
        ClassDef <- getClassDef(Class)
        ## an initial assignment prevents recursive looping should this class's
        ## definition be needed during the computations (such loops are usually but
        ## not quite always an error).
        environment(ClassDef) <- copyEnvironment(ClassDef)
        ## copy the environment so the completion will not be saved beyond the
        ## session.
        assignClassDef(Class, ClassDef, 0)
        on.exit(if(is.null(value)) removeClass(Class, where = 0), add=TRUE)
        ev <- environment(ClassDef)
        properties <- getProperties(ClassDef)
        immediate <- getExtends(ClassDef)
        ext <- getAllSuperClasses(ClassDef)## all the direct and indirect superClasses
        if(length(ext) > 0) {
            superProps <- list()
            for(eClass in rev(ext)) {
                classProps <- getProperties(getClass(eClass))
                superProps[names(classProps)] <- classProps
            }
            superProps[names(properties)] <- properties
            properties <- superProps
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
        assignClassDef(Class, ClassDef, 0)
        value <- ClassDef
    }
    else {
        ## create a class definition, possibly an empty virtual class
        prototype <- newBasic(Class, .Force=TRUE)
        ## newBasic never exactly returns NULL, but testVirtual uses NULL prototype
        ## as a requirement for a virtual class -- based on a problem with NULL in R,
        ## so may change.  See documentation for `new'
        if(is(prototype, "NULL"))
            prototype <- NULL
        setClass(Class, prototype = prototype, where = 0)
        value <- getClass(Class)
    }
    value
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
    immediate <- names(getExtends(ClassDef))
    super <- character()
    for(what in immediate)
      ## calling getClass will recursively force completion of the superclasses
      super <- c(super, what, names(getExtends(getClass(what))))
    unique(super)
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
    if(isClass(Class))
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
  ## Class="NULL" (disallowed because NULL can't have attributes).  For all cases except
  ## "NULL", the class of the result will be set to Class.
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
               "array" = (if(nargs() > 1) array(...) else structure(numeric(), .Dim =0)),
               "matrix" = (if(nargs() > 1) matrix(...) else matrix(0,0,0)),
               "ts" = ts(...),
            ## The language data
                  "name" = as.name("<UNDEFINED>"), # R won't allow 0 length names
                  "call" = quote({}), ## general expressions all get data.class=="call"
                  if(.Force)
                    ## create an empty object, even though this class is undefined
                  ## for why this is not NULL, see the documentation for `new'.
                  ## note that this is the only case where class is not set to Class
                    return(newEmptyObject())
                  else
                    stop(paste("Calling new() on an undefined and non-basic class (\"",
                               Class, "\")", sep=""))
                  )
  class(value) <- Class
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

reconcilePropertiesAndPrototype <-
  ## makes a list or a structure look like a prototype for the given class.
  ##
  ## Specifically, returns a structure with attributes corresponding to the slot
  ## names in properties and values taken from prototype if they exist there, from
  ## `new(classi)' for the class, `classi' of the slot if that succeeds, and `NULL'
  ## otherwise.
  ##
  ## The prototype may imply slots not in the properties list, since properties does not
  ## include inherited slots (these are left unresolved until the class is used in a
  ## session).
  function(name, properties, prototype) {
    Class <- data.class(prototype)
    if(identical(Class, "list")) {
      slots <- prototype
      prototype <- list()
    }
    else if(isClass(Class) && length(slotNames(Class))>0 && length(properties)>0) {
      slots <- attributes(unClass(prototype))
      prototype <- list()
    }
    else if(length(attributes(prototype))>1) {
      slots <- attributes(prototype)
      what <- match(names(slots), properties)
      slots <- slots[!is.na(what)]
      prototype <- list()
    }
    else
      slots <- list()
    ## now set slots in the prototype object.  An important detail is that these are
    ## set using slot<- with check=FALSE (because the slot won't be there already)
    ## Another related detail is that the value in the protype may be NULL.  If the supplied
    ## prototype was generated as a list, the NULL values aren't there.  This will only cause
    ## a problem if the default value for the corresponding class is not NULL.  In this case,
    ## the programmer needs to set the prototype element to nullSymbol(), which gets converted
    ## to NULL when returned as a slot.
    what <- names(slots)
    pnames <- names(properties)
    for(i in seq(along=what)) {
      prop <- el(what, i)
      j <- match(prop, pnames)
      if(!is.na(j))
        sloti <- as(el(slots,i), el(properties, j))
      else
        sloti <- el(slots, i)
      slot(prototype, prop, FALSE) <- sloti
    }
    for(i in seq(along=pnames)) {
      prop <- el(pnames, i)
      if(is.na(match(prop, what)))
        slot(prototype, prop, FALSE) <- tryNew(el(properties, i))
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


showClass <-
  ## print the information about a class definition.  If complete==TRUE, include the
  ## indirect information about extensions.
  function(Class, complete = TRUE, propertiesAreCalled = "Properties") {
    if(is(Class, "classRepEnvironment")) {
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
      cat("\n",propertiesAreCalled, ":\n", sep="")
      xx <- as.character(x)
      names(xx) <- names(x)
      print(xx)
    }
    else
      cat("\nNo ", propertiesAreCalled, ", prototype of class \"",
          data.class(getPrototype(ClassDef)), "\"\n", sep="")
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
  function(ext) {
    what <- names(ext)
    for(i in seq(along=ext)) {
      cat("Class \"", el(what, i), "\" ", sep="")
      eli <- el(ext, i)
      if(length(eli$by) > 0)
        how <- paste("by class", paste("\"", eli$by, "\"", sep="", collapse = ", "))
      else
        how <- "directly"
      if(is.function(eli$test)) {
        if(is.function(eli$coerce))
          how <- paste(how, "with explicit test and coerce")
        else
          how <- pate(how, "with explicit test")
      }
      else if(is.function(eli$coerce))
        how <- paste(how, "with explicit coerce")
      cat(how, "\n")
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

unsetClass <-
  ## remove this class name from the internal table for this session.
  ##
  ## Rarely needed to call this directly.
  function(Class)
{
    Class <- classMetaName(Class)
    if(!is.null(getFromClassMetaData(Class)))
        removeFromClassMetaData(Class)
}


extendsCoerce <-
  ## the function to perform coercion based on the is relation
  ## between two classes.  May be explicitly stored in the metadata or
  ## inferred.  If the latter, the inferred result is stored in the session
  ## metadata for fromClass, to save recomputation later.
  function(fromClass, Class)
{
    ext <- findExtends(fromClass, Class)
    f <- NULL
    if(is.list(ext)) {
        coe <- ext$coerce
        if(is.function(coe))
            return(coe)
        by <- list$by
        if(length(by) > 0)
            f <- substitute(function(object)
                            as(as(object, BY), CLASS),
                            list(BY = by, CLASS=Class))
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
                f <- substitute(function(object){data.class(object) <- CLASS; object},
                                list(CLASS = Class))
            else
                f <- substitute(function(object) {
                    value <- new(CLASS)
                    for(what in TOSLOTS)
                        slot(value, what) <- slot(object, what)
                    value }, list(CLASS=Class, TOSLOTS = toSlots))
            ## bug in R: substitute of a function gives a call
            mode(f) <- "function"
        }
        ## we dropped through because there was no coerce function in the
        ## extends object.  Make one and save it back in the session metadata
        ## so no further calls will require constructing the function
        if(!is.list(ext))
            ext <- list()
        ext$coerce <- f
        ClassDef <- getClass(fromClass)
        allExt <- as.list(getExtends(ClassDef))
        allExt[Class] <- ext
        setExtends(ClassDef, allExt)
    }
    f
}

extendsReplace <-
  ## the function to perform as() replacement based on the is relation
  ## between two classes.  May be explicitly stored in the metadata or
  ## inferred.  If the latter, the inferred result is stored in the session
  ## metadata for fromClass, to save recomputation later.
  function(fromClass, Class)
{
    ext <- findExtends(fromClass, Class)
    f <- NULL
    if(is.list(ext)) {
        repl <- ext$replace
        if(is.function(repl))
            return(repl)
        by <- list$by
        if(length(by) > 0) {
          f <- substitute(function(object, value)
                          as(as(object, BY), CLASS) <- value,
                          list(BY = by, CLASS=Class))
          f <- eval(f, .GlobalEnv)
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
                f <- substitute(function(object, value) {
                     for(what in TOSLOTS)
                        slot(object, what) <- slot(value, what)
                    object }, list(CLASS=Class, TOSLOTS = toSlots))
            f <- eval(f, .GlobalEnv)
        }
        ## we dropped through because there was no replace function in the
        ## extends object.  Make one and save it back in the session metadata
        ## so no further calls will require constructing the function
        if(!is.list(ext))
            ext <- list()
        ext$replace <- f  ## might be NULL
        ClassDef <- getClass(fromClass)
        allExt <- as.list(getExtends(ClassDef))
        allExt[Class] <- ext
        setExtends(ClassDef, allExt)
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
        value <- el(ext, i)
        if(is.list(value))
            value
        else TRUE
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
  function(ClassDef)
{
    ext <- getExtends(ClassDef)
    what <- names(ext)
    test <- sapply(ext, function(obj)is.list(obj) && is.function(obj$test))
    value <- list()
    for(i in seq(along=ext)) {
        by <- el(what, i)
        valueEl <- ext[i]               ## note: an extra level of list, to be unlisted later
        if(isClass(by))
        {
            more <- completeExtends(getClass(by))
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
  classDef <- if(complete) getClass(x) else getClassDef(x)
  props <- getProperties(classDef)
  value <- as(props, "character")
  names(value) <- names(props)
  value
}
