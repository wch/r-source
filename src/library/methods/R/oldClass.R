#  File src/library/methods/R/oldClass.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

## assumes oldClass has been defined as a virtual class

setOldClass <- function(Classes, prototype = NULL,
                        where = topenv(parent.frame()), test = FALSE,
                        S4Class) {
    simpleCase <- is.null(prototype)
    mainClass <- Classes[[1L]]
    prevDef <- getClassDef(mainClass, where, inherits = FALSE)
    if(!missing(S4Class)) {
        if(test)
          stop("not allowed to have test==TRUE and an S4Class definition")
        if(!is(S4Class, "classRepresentation")) {
            if(is.character(S4Class)) {
                clName <- S4Class
                S4Class <- getClass(S4Class)
                if(.identC(clName, Classes[[1L]]))
                  removeClass(clName, where = where) # so Recall() will work
            }
            else
              stop(gettextf("argument 'S4Class' must be a class definition: got an object of class %s",
                            dQuote(class(S4Class))),
                   domain = NA)
        }
        if(!is.null(prototype)) {
            S4prototype <- S4Class@prototype
            ## use the explicit attributes from the supplied argument, else S4prototype
            S4Class@prototype <- .mergeAttrs(prototype, S4prototype)
        }
        ## register simple S3 class(es), including main class, if it's not defined already
        Recall(Classes, where = where)
        return(.S4OldClass(Classes[[1L]], if(length(Classes) > 1) Classes[[2L]] else "oldClass", S4Class, where, prevDef))
    }
    if(test)
        return(.setOldIs(Classes, where))
    if(!is.null(prevDef)) {
        on.exit(.restoreClass(prevDef, where))
        removeClass(mainClass, where = where) # so Recall() will work
    }
    prevClass <- "oldClass"
    S3Class <- character()  #will accumulate the S3 classes inherited
    ## The table of S3 classes, used
    ## to convert S4 objects in S3 method dispatch.
    ## TODO:  should provide an optional argument to setOldClass()
    ## to prevednt this conversion if it's not needed
    if(is.null(S3table <- where$.S3MethodsClasses)) {
      S3table <- new.env()
      assign(".S3MethodsClasses", S3table, envir = where)
    }
    dataPartClass <- NULL
    for(cl in rev(Classes)) {
       S3Class <- c(cl, S3Class)
        if(isClass(cl, where)) {
            def <- getClass(cl, where)
            if(!extends(def, prevClass)) {
                ## maybe an object type or other valid data part
                cl1 <- .validDataPartClass(cl, where, dataPartClass)
                if(is.null(cl1))
                  stop(gettextf("inconsistent old-style class information for %s; the class is defined but does not extend %s and is not valid as the data part",
                                dQuote(cl),
                                dQuote(prevClass)),
                       domain = NA)
                else dataPartClass <- cl1
              }
            else {
              prevP <- def@prototype
              if(missing(prototype))
                prototype <- prevP # keep track of inherited prototype for use in mainClass
              prevS3Class <- attr(prevP, ".S3Class")
              if(length(prevS3Class) > length(S3Class)) #implies cl is registered S3 class
                S3Class <- prevS3Class
            }
        }
        else {
            useP <- TRUE
            if(cl != mainClass || simpleCase) {
                setClass(cl, contains = c(prevClass, "VIRTUAL"), where = where)
            }
            else if(isClass(class(prototype)))
                setClass(cl, contains = prevClass, prototype = prototype, where = where)
            else { #exceptionally, we allow an S3 object from the S3 class as prototype
                if(.class1(prototype) != mainClass)
                  stop(gettextf('the S3 class of the prototype, "%s", is undefined; only allowed when this is the S3 class being registered ("%s")', .class1(prototype), mainClass), domain = NA)
                setClass(cl, contains = prevClass, where = where)
                useP <- FALSE
            }
            def <- getClassDef(cl, where)
            if(useP) clp <- def@prototype else clp <- prototype
            attr(clp, ".S3Class") <- S3Class
            def@prototype <- .notS4(clp)
            assignClassDef(cl, def, where = where)
            ## add the class to the table of S3 classes
            assign(cl, def, envir= S3table)
        }
       prevClass <- cl
    }
    if(!is.null(prevDef)) # cancel error action
      on.exit()
}

.restoreClass <- function(def, where) {
    cl <- def@className
    message(gettextf("restoring definition of class %s", dQuote(cl)),
            domain = NA)
    if(isClass(cl, where = where))
       removeClass(cl, where = where)
    assignClassDef(cl, def, where = where)
}

.S4OldClass <- function(Class, prevClass, def,where, prevDef) {
    ## def is the S4 version of this class def'n, maybe by another class
    ## name, and may or may not already extend oldClass
    curDef <- getClassDef(Class, where) # asserted to be defined
    ## arrange to restore previous definition if there was one.  Also done in setOldClass
    ## when no S4Class argument supplied
    if(!is.null(prevDef)) {
        on.exit(.restoreClass(prevDef, where))
        removeClass(Class, where = where) # so Recall() will work
    }
    if(!identical(def@className, curDef@className))
      def <- .renameClassDef(def, curDef@className)
    ## check that any common slots will give a valid S3 object
    .validS3Extends(def, curDef)
    def@slots <- c(def@slots, curDef@slots)
    ext <- c(def@contains, curDef@contains)
    ## correct ordering & duplicate resolution: copied from .walkClassGraph
    distOrder <- sort.list(vapply(ext, function(x) x@distance, 1))
    ext <- ext[distOrder]
    if(anyDuplicated(names(ext)))
        ext <- .resolveSuperclasses(def, ext, where)
    def@contains <- ext
    subcls <- curDef@subclasses
    if(length(subcls) > 0) {
      def@subclasses[names(subcls)]  <- subcls
    }
    proto <- def@prototype
    if(is.null(attr(proto, ".S3Class"))) { # no S3 class slot, as will usually be true
        attr(proto, ".S3Class") <- if(.identC(prevClass, "oldClass")) Class else S3Class(curDef@prototype)
        def@prototype <- proto
    }
    assignClassDef(Class, def, where = where)
    ## allow an existing superclass relation to remain (it may have a coerce method)
    ## Otherwise, create a simple transformation, which relies on consistency
    ## in the slots.
    if(!extends(def, prevClass, maybe = FALSE))
      setIs(Class, prevClass, classDef = def, where = where)
    slotsMethod <- function(object) NULL
    body(slotsMethod) <- substitute({LIST}, list(LIST = def@slots))
    setMethod("slotsFromS3", Class, slotsMethod, where = where)
    if(!is.null(prevDef)) # cancel error action
      on.exit()
}

.validS3Extends <- function(classDef1, classDef2) {
    slots2 <- classDef2@slots
    if(length(slots2) > 0) {
        n2 <- names(slots2)
        slots1 <- classDef1@slots
        n1 <- names(slots1)
        bad <- character()
        for(what in n2[match(n2, n1, 0) > 0])
          if(!extends(elNamed(slots1, what), elNamed(slots2, what))) {
              message(gettextf("slot %s: class %s should extend class %s",
                               sQuote(what),
                               dQuote(elNamed(slots1, what)),
                               dQuote(elNamed(slots2, what))),
                      domain = NA)
              bad <- c(bad, what)
          }
        if(length(bad)>0)
          stop(
               gettextf("invalid S4 class corresponding to S3 class: slots in  S4 version must extend corresponding slots in S3 version: fails for %s",
                        paste0('"', bad, '"',  collapse = ", ")),
               domain = NA)
    }
    TRUE
}

##.initS3Classes will make this generic, with a method for "oldClass"
slotsFromS3 <- function(object) {
    list()
}

utils::globalVariables("CLASS")

.oldTestFun <- function(object) CLASS %in% attr(object, "class")
.oldCoerceFun <- function(from, strict = TRUE) {
    if(strict)
        stop(gettextf("explicit coercion of old-style class (%s) is not defined", paste(class(from), collapse = ", ")), domain = NA)
    from
}
.oldReplaceFun <- function(from, to, value)
    stop(gettextf("explicit replacement not defined for as(x, \"%s\") <- value for old-style class %s",
                  to, dQuote(class(from)[1L])),
         domain = NA)

## the inheritance of these S3 classes must be decided on a per-instance
## basis.  At one time, there were classes in base/stats that had this
## property, (e.g., POSIXt, POSIX{cl}t) but apparently no longer.
## The possibility is still allowed
## for user-defined S3 classes.
.setOldIs <- function(Classes, where) {
    if(length(Classes) != 2)
        stop(gettextf("argument 'Classes' must be a vector of two classes; got an argument of length %d", length(Classes)), domain = NA)
    for(cl in Classes) {
        if(isClass(cl, where)) {
            if(!extends(cl, "oldClass"))
                warning(gettextf("inconsistent old-style class information for %s (maybe mixing old and new classes?)",
                                 dQuote(cl)), domain = NA)
        }
        else
            setClass(cl, representation("oldClass", "VIRTUAL"), where = where)
    }
    Class1 <- Classes[[1L]]
    for(cl in Classes[-1L]) {
        tfun <- .oldTestFun
        body(tfun, envir = environment(tfun)) <-
            substitute(inherits(object, CLASS), list(CLASS = cl))
        setIs(Class1, cl, test = tfun, coerce = .oldCoerceFun,
              replace = .oldReplaceFun, where = where)
    }
    NULL
}

isXS3Class <- function(classDef) {
    ".S3Class" %in% names(classDef@slots)
}

S3Class <- function(object) {
    value <- attr(object, ".S3Class")
    if(is.null(value)) {
        if(isS4(object)) {
            if(is.na(match(".Data", names(getClass(class(object))@slots))))
                stop(gettextf("'S3Class' only defined for extensions of %s or classes with a data part:  not true of class %s",
                              dQuote("oldClass"),
                              dQuote(class(object))),
                     domain = NA)
            class(getDataPart(object))
        }
        else
          class(object)
    }
    else
      value
}

.S3Class <- S3Class # alias for functions with S3Class as an argument

.addS3Class <- function(class, prototype, contains, where) {
    for(what in contains) {
        whatDef <- getClassDef(what@superClass, where = where)
        if(isXS3Class(whatDef))
          class <- c(class, attr(whatDef@prototype, ".S3Class"))
    }
    attr(prototype, ".S3Class") <- unique(class)
    prototype
}

"S3Class<-" <- function(object, value) {
    if(isS4(object)) {
        current <- attr(object, ".S3Class")
        if(is.null(current)) {
            if(is.na(match(value, .BasicClasses)))
               stop(gettextf("'S3Class' can only assign to S4 objects that extend \"oldClass\"; not true of class %s",
                             dQuote(class(object))),
                    domain = NA)
            mode(object) <- value ## may still fail, a further check would be good
        }
        else
          slot(object, ".S3Class") <- value
    }
    else
      class(object) <- value
    object
}

## rename a class definition:  needs to change if any additional occurences of class
## name are added, other than the className slot and the super/sub class names
## in the contains, subclasses slots respectively.
.renameClassDef <- function(def, className) {
    oldName <- def@className
    validObject(def) # to catch any non-SClassExtension objects
    def@className <- className
    comp <- def@contains
    for(i in seq_along(comp))
        comp[[i]]@subClass <- className
    def@contains <- comp
    comp <- def@subclasses
    for(i in seq_along(comp))
        comp[[i]]@superClass <- className
    def@subclasses <- comp
    def
}

## extends() w/o conditional inheritance:  used for S3 inheritance, method
## selection on S4 objects
..extendsForS3 <- function(Class)
    extends(Class, maybe = FALSE)
## dummy version while generating methods package
.extendsForS3 <- function(Class)
    extends(Class)
