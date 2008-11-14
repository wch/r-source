#  File src/library/methods/R/oldClass.R
#  Part of the R package, http://www.R-project.org
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
    if(!missing(S4Class)) {
        if(test)
          stop("not allowed to have test==TRUE and an S4Class definition")
        if(!is(S4Class, "classRepresentation")) {
            if(is.character(S4Class)) {
                clName <- S4Class
                S4Class <- getClass(S4Class)
                if(.identC(clName, Classes[[1]]))
                  removeClass(clName, where = where) # so Recall() will work
            }
            else
              stop(gettextf("argument S4Class must be a class definition:  got an object of class \"%s\"", class(S4Class)))
        }
        if(!is.null(prototype)) {
            S4prototype <- S4Class@prototype
            ## use the explicit attributes from the supplied argument, else S4prototype
            S4Class@prototype <- .mergeAttrs(prototype, S4prototype)
        }
        ## register simple S3 class(es), including main class, if it's not defined already
        Recall(Classes, where = where)
        return(.S4OldClass(Classes[[1]], if(length(Classes) > 1) Classes[[2]] else "oldClass", S4Class, where))
    }
    if(test)
        return(.setOldIs(Classes, where))
    mainClass <- Classes[[1]]
    prevClass <- "oldClass"
    S3Class <- character()  #will accumulate the S3 classes inherited
    for(cl in rev(Classes)) {
       S3Class <- c(cl, S3Class)
        if(isClass(cl, where)) {
            def <- getClass(cl, where)
            if(!extends(def, prevClass))
                stop(gettextf("inconsistent old-style class information for \"%s\" (maybe mixing old and new classes?)", cl), domain = NA)
            prevP <- def@prototype
            if(missing(prototype))
              prototype <- prevP # keep track of inherited prototype for use in mainClass
            prevS3Class <- attr(prevP, ".S3Class")
            if(length(prevS3Class) > length(S3Class)) #implies cl is registered S3 class
                S3Class <- prevS3Class
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
                  stop(gettextf('The S3 class of the prototype, "%s", is undefined; only allowed when this is the S3 class being registered ("%s")', .class1(prototype), mainClass), domain = NA)
                setClass(cl, contains = prevClass, where = where)
                useP <- FALSE
            }
            def <- getClassDef(cl, where)
            if(useP) clp <- def@prototype else clp <- prototype
            attr(clp, ".S3Class") <- S3Class
            def@prototype <- .notS4(clp)
            assignClassDef(cl, def, where = where)
        }
       prevClass <- cl
    }
}

.S4OldClass <- function(class, prevClass, def,where) {
    curDef <- getClassDef(class, where) # asserted to be defined
    if(!identical(def@className, curDef@className))
      def <- .renameClassDef(def, curDef@className)
    def@slots <- c(def@slots, curDef@slots)
    proto <- def@prototype
    if(is.null(attr(proto, ".S3Class"))) { # no S3 clas slot, as will usually be true
        attr(proto, ".S3Class") <- class
        def@prototype <- proto
    }
    assignClassDef(class, def, where = where)
    setIs(class, prevClass, classDef = def, where = where)
    slotsMethod <- function(object) NULL
    body(slotsMethod) <- substitute({LIST}, list(LIST = def@slots))
    setMethod("slotsFromS3", class, slotsMethod, where = where)
}

##.initS3Classes will make this generic, with a method for "oldClass"
slotsFromS3 <- function(object) {
    list()
}

.oldTestFun <- function(object) CLASS %in% attr(object, "class")
.oldCoerceFun <- function(from, strict = TRUE) {
    if(strict)
        stop(gettextf("explicit coercion of old-style class (%s) is not defined", paste(class(from), collapse = ", ")), domain = NA)
    from
}
.oldReplaceFun <- function(from, to, value)
    stop(gettextf("explicit replacement not defined for as(x, \"%s\") <- value for old-style class \"%s\"", to, class(from)[1]), domain = NA)

.setOldIs <- function(Classes, where) {
    if(length(Classes) != 2)
        stop(gettextf("argument 'Classes' must be a vector of two classes; got an argument of length %d", length(Classes)), domain = NA)
    for(cl in Classes) {
        if(isClass(cl, where)) {
            if(!extends(cl, "oldClass"))
                warning(gettextf("inconsistent old-style class information for \"%s\" (maybe mixing old and new classes?)", cl), domain = NA)
        }
        else
            setClass(cl, representation("oldClass", "VIRTUAL"), where = where)
    }
    Class1 <- Classes[[1]]
    for(cl in Classes[-1]) {
        tfun <- .oldTestFun
        body(tfun, envir = environment(tfun)) <-
            substitute(CLASS %in% attr(object, "class"), list(CLASS = cl))
        setIs(Class1, cl, test = tfun, coerce = .oldCoerceFun,
              replace = .oldReplaceFun, where = where)
    }
}

isXS3Class <- function(classDef) {
    ".S3Class" %in% names(classDef@slots)
}

S3Class <- function(object) {
    value <- attr(object, ".S3Class")
    if(is.null(value)) {
        if(isS4(object)) {
            if(is.na(match(".Data", names(getClass(class(object))@slots))))
              stop("S3Class only defined for extensions of \"oldClass\" or classes with a data part:  not true of class \"", class(object), "\"")
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
               stop(gettextf("S3Class can only be assigned to S4 objects that extend \"oldClass\"; not true of class \"%s\"",
                        class(object)), domain = NA)
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
    
