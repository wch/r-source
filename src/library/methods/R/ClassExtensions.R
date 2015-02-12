#  File src/library/methods/R/ClassExtensions.R
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

.InitExtensions <- function(where) {
    ## to be called from the initialization
    setClass("SClassExtension",
	     representation(subClass = "character", superClass = "character",
			    package = "character", coerce = "function",
			    test = "function", replace = "function",
			    simple = "logical", by = "character",
			    dataPart = "logical", distance = "numeric"),
	     where = where)
    ## a class for conditional extensions, so they will not break the hierarchical
    ## structure.
    setClass("conditionalExtension", contains = "SClassExtension")
    assign(".SealedClasses", c(get(".SealedClasses", where), "SClassExtension",
                               "conditionalExtension"),
	   where)
}

.simpleExtCoerce <- function(from, strict = TRUE)from
.simpleIsCoerce <- function(from)from
.simpleExtTest <- function(object)TRUE
## TO DO:  the simple replace below only handles the case of classes with slots.
## There are some other simple relations (e.g., with virtual classes).  Replacing in
## these cases is less likely but needs to be tested (below) and a suitable
## replace function inserted.
.simpleExtReplace <- function(from, to, value){
    for(what in .InhSlotNames(to))
        slot(from, what) <- slot(value, what)
    from
}
## slot names for inheritance (to be used in replace methods).  Extends slots to implicit
## .Data for basic classes.
.InhSlotNames <- function(Class) {
   ClassDef <- getClass(Class)
    value <- names(ClassDef@slots)
    if(length(value)==0 && (Class %in% .BasicClasses || extends(ClassDef, "vector")))
        ## No slots, but extends "vector" => usually a basic class; treat as data part
        value <- ".Data"
   value
}
.dataPartReplace <- list(f1 = function(from, to, value){
    from@.Data <- value
    from
},

f2 = function(from, to, value){
    from@.Data <- as(value, THISCLASS, strict = FALSE)
    from
},

## and a version of dataPartReplace w/o the unused `to' argument
f2args = function(from, value) {
    from@.Data <- value
    from
})

S3Part <- function(object, strictS3 = FALSE, S3Class) {
    if(!isS4(object))
      return(object)
    classDef <- getClass(class(object))
    oldClassCase <- extends(classDef, "oldClass")
    defltS3Class <- missing(S3Class)
    if(oldClassCase) {
        if(defltS3Class)
            S3Class <- .S3Class(object)
        keepSlots <- slotNames(S3Class[[1L]])
     }
    else {
        if(all(is.na(match(extends(classDef), .BasicClasses))))
          stop(gettextf("S3Part() is only defined for classes set up by setOldCLass(), basic classes or subclasses of these:  not true of class %s", dQuote(class(object))), domain = NA)
        if(missing(S3Class)) {
            S3Class <- classDef@slots$.Data
            if(is.null(S3Class)) # is this an error?
              S3Class <- typeof(object)
            keepSlots <- character()
        }
        else
          keepSlots <- slotNames(S3Class[[1L]])
    }
    if(!(defltS3Class || extends(classDef, S3Class)))
      stop(gettextf("the 'S3Class' argument must be a superclass of %s:  not true of class %s", dQuote(class(object)), dQuote(S3Class)), domain = NA)
    if(strictS3)
      keepSlots <- keepSlots[is.na(match(keepSlots, ".S3Class"))]
    deleteSlots = slotNames(classDef)
    deleteSlots <- deleteSlots[is.na(match(deleteSlots,keepSlots))]
    for(slot in deleteSlots)
      attr(object, slot) <- NULL
    if(strictS3) {
        object <- .notS4(object)
        class(object) <- S3Class
    }
    else
      class(object) <- S3Class[[1L]]
    object
}

"S3Part<-" <- function(object, strictS3 = FALSE, needClass = .S3Class(object) , value) {
    S3Class <- .S3Class(value)
    def <- getClassDef(S3Class[[1L]])
    if(is.null(def) || !extends(def, needClass[[1L]]))
      stop(gettextf("replacement value must extend class %s, got %s", dQuote(needClass), dQuote(S3Class[[1L]])), domain = NA)
    slots <- slotNames(class(object))
    if(!strictS3) {
        fromValue <- names(attributes(value))
        slots <- slots[is.na(match(slots, fromValue))]
    }
    slots <- c("class", slots)  # always preserve class(object)
    for(slot in slots)
      attr(value, slot) <- attr(object, slot)
    if(extends(def, "oldClass"))
      attr(value, ".S3Class") <- S3Class
    if(isS4(object))
      value <- .asS4(value)
    value
}

## templates for replacement methods for S3 classes in classes that extend oldClass
.S3replace <-
    list(e1 =
         quote( {
             S3Part(from, needClass = NEED) <- value
             from
         }),
         e2 = quote( {
             if(is(value, CLASS)) {
                 S3Part(from,  needClass = NEED) <- value
                 from
             }
             else
                 stop(gettextf("replacement value must be of class %s, got one of class %s",
                               dQuote(CLASS),
                               dQuote(class(value)[[1L]])))

         })
         )

.S3coerce <- function(from, to) {
    S3Part(from)
}

.ErrorReplace <- function(from, to, value)
    stop(gettextf("no 'replace' method was defined for 'as(x, \"%s\") <- value' for class %s",
                  to, dQuote(class(from))), domain = NA)

.objectSlotNames <- function(object) {
    ## a quick version that makes no attempt to check the class definition
    value <- names(attributes(object))
    if(is.null(value)) ## not possible with methods package?
        character()
    else
        value[-match("class", value, 0L)]
}

makeExtends <- function(Class, to,
                        coerce = NULL, test = NULL, replace = NULL,
                        by = character(), package,
                        slots = getSlots(classDef1),
                        classDef1 = getClass(Class), classDef2) {
    ## test for datapart class:  must be the data part class, except
    ## that extensions within the basic classes are allowed (numeric, integer)
    dataEquiv <- function(cl1, cl2) {
        .identC(cl1, cl2) ||
          (extends(cl1, cl2) && !any(is.na(match(c(cl1, cl2), .BasicClasses))))
    }
    packageEnv <- .requirePackage(package)
    class1Defined <- missing(slots) # only at this time can we construct methods
    simple <- is.null(coerce) && is.null(test) && is.null(replace) && (length(by)==0)
    distance <- 1
    ##FIX ME:  when by is supplied, should use the existing extension information
    ## to compute distance
    dataPartClass <- elNamed(slots, ".Data")
    dataPart <- FALSE
    if(simple && !is.null(dataPartClass)) {
        if(!(is.null(getClassDef(dataPartClass)) || is.null(getClassDef(to)))) {
            ## note that dataPart, to are looked up in the methods package & parents,
            ## because the default in getClassDef is the topenv of the caller (this fun.):
            ## Assertion is that only these classes are allowed as data slots
            dataPart <- dataEquiv(dataPartClass, to)
        }
    }
    if(is.null(coerce)) {
        coerce <- .simpleExtCoerce
        if(isXS3Class(classDef2)) {
##            allNames <- names(slots)
            body(coerce, envir = packageEnv) <-
                substitute({
                    if(strict) S3Part(from, S3Class = S3CLASS)
                    else from
                }, list(S3CLASS =  to))
        }
        else if(!isVirtualClass(classDef2))
            body(coerce, envir = packageEnv) <-
                 .simpleCoerceExpr(Class, to, names(slots), classDef2)
    }
    else if(is(coerce, "function")) {
        ## we allow definitions with and without the `strict' argument
        ## but create a  function that can be called with the argument
        if(length(formals(coerce)) == 1) {
            coerce <- .ChangeFormals(coerce, .simpleIsCoerce, "'coerce' argument to setIs ")
            tmp <- .simpleExtCoerce
            body(tmp, envir = environment(coerce)) <- body(coerce)
            coerce <- tmp
        }
        else
            coerce <- .ChangeFormals(coerce, .simpleExtCoerce, "'coerce' argument to setIs ")

    }
    else stop(gettextf("the 'coerce' argument to 'setIs' should be a function of one argument, got an object of class %s",
                       dQuote(class(coerce))), domain = NA)
    if(is.null(test)) {
        test <- .simpleExtTest
        extClass <- "SClassExtension"
    }
    else {
        test <- .ChangeFormals(test, .simpleExtTest, "'test' argument to setIs ")
        extClass <- "conditionalExtension"
    }
    if(is.null(replace)) {
        if(dataPart) {
            extn <- elNamed(classDef2@contains, dataPartClass)
            if(is(extn, "SClassExtension"))
                easy <- extn@simple
            else
                easy <- FALSE
            if(easy)
                replace <- .dataPartReplace$f1
            else {
                replace <- .dataPartReplace$f2
                bdy <- body(replace)
                body(replace, envir = environment(replace)) <-
                    substituteDirect(bdy, list(THISCLASS = dataPartClass))
            }
        }
        else if(simple) {
            replace <- .simpleExtReplace
            if(isXS3Class(classDef2)) {  # replace the S3 part & slots in class to
                S3Class <- attr(classDef2@prototype, ".S3Class")
                if(is.null(S3Class)) # the setOldClass case ?
                  S3Class <- to
                body(replace, envir = packageEnv) <-
                  quote({
                      S3Part(from) <- value
                      from
                  })
            }
            else if(isVirtualClass(classDef2)) {  # a simple is to a virtual class => a union
                body(replace, envir = packageEnv) <-
                    substitute({
                        if(!is(value, TO))
                            stop(gettextf("the computation: 'as(object,\"%s\") <- value' is valid when object has class %s only if 'is(value, \"%s\")' is TRUE ('class(value)' was %s)\n",
                                 TO, dQuote(FROM), TO, dQuote(class(value))), domain = NA)
                        value
                    }, list(FROM = Class, TO = to))
            }
            else if(class1Defined && length(slots) == 0) {
                ## check for the classes having the same representation
                ## (including the case of no slots)
                ext <- getAllSuperClasses(classDef1, TRUE)
                toSlots <- classDef2@slots
                sameSlots <- TRUE
                for(eclass in ext) {
                    ## does any superclass other than "to" have slots?
                    if(.identC(eclass, to))
                        next
                    edef <- getClassDef(eclass, where = packageEnv)
                    if(!is.null(edef) && length(edef@slots) > 0) {
                        sameSlots <- FALSE
                        break
                    }
                }
                if(sameSlots)
                    body(replace, envir = packageEnv) <-
                        substitute({class(value) <- FROM; value}, list(FROM = Class))
                else if(length(toSlots) == 0) # seems replacement not defined in this case?
                    replace <- .ErrorReplace
            }
            else
                body(replace, envir = packageEnv) <-
                    .simpleReplaceExpr(classDef2)
        }
        else
            replace <- .ErrorReplace
        if(identical(replace, .ErrorReplace))
            warning(gettextf("there is no automatic definition for 'as(object, \"%s\") <- value' when object has class %s and no 'replace' argument was supplied; replacement will be an error",
                             to, dQuote(Class)), domain = NA)
    }
    else if(is(replace, "function")) {
        ## turn function of two or three arguments into correct 3-arg form
        if(length(formals(replace)) == 2) {
            replace <- .ChangeFormals(replace, .dataPartReplace$f2args, "'replace' argument to setIs ")
            tmp  <- .ErrorReplace
            body(tmp, envir = environment(replace)) <- body(replace)
            replace <- tmp
        }
        else
            replace <- .ChangeFormals(replace, .ErrorReplace, "'replace' argument to setIs ")
    }
    else
        stop(gettextf("the 'replace' argument to setIs() should be a function of 2 or 3 arguments, got an object of class %s",
                      dQuote(class(replace))), domain = NA)

    new(extClass, subClass = Class, superClass = to, package = package,
	coerce = coerce, test = test, replace = replace, simple = simple,
	by = by, dataPart = dataPart, distance = distance)
}

.findAll <- function(what, where = topenv(parent.frame())) {
    ## search in envir. & parents thereof
    ## For namespaces, this follows R's soft namespace policy
    ## by not stopping when it reaches the basenamespace
    ## The code used to do so and then had a kludge for looking
    ## in the methods namespace.  But that failed anyway on
    ## non-namespace (package) environments and was inconsistent
    ## with the normal R lookup with namespace environments.
    value <- list()
    if(is.environment(where)) {
        if(isNamespace(where)) repeat {
            if(exists(what, where, inherits = FALSE))
                value <- c(value, list(where))
            if(identical(where, emptyenv()))
                break
            where <- parent.env(where)
        }
        else {  # typically, a package environment: look here, then in the search list
            if(exists(what, where, inherits = FALSE))
                value <- c(value, list(where))
            for(i in seq_along(search())) {
                if(exists(what, i, inherits = FALSE)) {
                    evi <- as.environment(i)
                    addMe<- TRUE
                    for(other in value)
                        if(identical(other, evi)) {
                            addMe <- FALSE
                            break
                        }
                    if(addMe)
                        value <- c(value, list(evi))
                }
            }
        }
    }
    else
        for(i in where) {
            if(exists(what, i, inherits = FALSE))
                value <- c(value, list(i))
        }
    value
}

.S4inherits <- function(x, what, which) {
    superClasses <- extends(getClass(class(x)))
    if(which)
       match(what, superClasses, 0L)
    else
      what %in% superClasses
}

## find the S3 classes or their extensions in the indirect superclasses
## and give them the correct coerce and replacement methods
.S3Extends <- function(ClassDef, exts, where) {
    superClasses <- names(exts)
    S3Class <- attr(ClassDef@prototype, ".S3Class")
    need <- S3Class[[1L]]
    for(i in seq_along(exts)) {
        exti <- exts[[i]]
        if(exti@distance == 1)
            next # asserted that this was done by makeExtends
        what <- superClasses[[i]]
        whatDef <- getClassDef(what, where)
        if(is.null(whatDef) # but shouldn't happen,
           || !isXS3Class(whatDef))
            next
        coerce <- exti@coerce
        body(coerce, environment(coerce))<- body(.S3coerce)
        exti@coerce <- coerce
        replace <- exti@replace
        pos <- match(what, S3Class, 0L)
        if(pos > 1) # not the complete S3 class, probably an error
          body(replace, environment(replace)) <-
            substituteDirect(.S3replace$e2, list(CLASS = what, NEED = need))
        else
          body(replace, environment(replace))  <-
            substituteDirect(.S3replace$e1, list(NEED = need))
        exti@replace <- replace
        exts[[i]] <- exti
    }
    exts
}

