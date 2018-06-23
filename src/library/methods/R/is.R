#  File src/library/methods/R/is.R
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


is <- function(object, class2)
{
  # With two arguments, tests whether `object' can be treated as from `class2'.
  #
  # With one argument, returns all the super-classes of this object's class.
    class1 <- class(object)
    S3Case <- length(class1) > 1L
    if(S3Case)
        class1 <- class1[[1L]]
    if(missing(class2))
        return(extends(class1))
    class1Def <- getClassDef(class1)
    if(is.null(class1Def)) # an unregistered S3 class
        return(inherits(object, class2))
    if(is.character(class2)) {
        package <- packageSlot(class2)
        if (is.null(package)) {
            package <- getPackageName(topenv(parent.frame()))
        }
        class2Def <- getClassDef(class2, .classDefEnv(class1Def), package)
    }
    else {
        class2Def <- class2
        class2 <- class2Def@ className
    }
    ## S3 inheritance is applied if the object is not S4 and class2 is either
    ## a basic class or an S3 class (registered or not)
    S3Case <- S3Case || (is.object(object) && !isS4(object))
    S3Case <- S3Case && (is.null(class2Def) || class2 %in% .BasicClasses ||
                         extends(class2Def, "oldClass"))
    if(S3Case)
        inherits(object, class2)
    else if(.identC(class1, class2) || .identC(class2, "ANY"))
        TRUE
    else { ## look for class1 in the known subclasses of class2
        if(!is.null(contained <- class1Def@contains[[class2]]))
            contained@simple || contained@test(object)
        else if (is.null(class2Def))
            FALSE
        else if(!.identC(class(class2Def), "classRepresentation") &&
                isClassUnion(class2Def))
            any(c(class1, names(class1Def@contains)) %in%
                names(class2Def@subclasses))
        else {
            ext <- class2Def@subclasses[[class1]]
            !is.null(ext) && (ext@simple || ext@test(object))
        }
    }
}

extends <-
  ## Does the first class extend the second class?
  ## Returns `maybe' if the extension includes a non-trivial test.
  function(class1, class2, maybe = TRUE, fullInfo = FALSE)
{
    if(is.character(class1)) {
        if(length(class1) > 1L)
            class1 <- class1[[1L]]
	classDef1 <- getClassDef(class1)
    } else if(is(class1, "classRepresentation")) {
	classDef1 <- class1
	class1 <- classDef1@className
    }
    else
	stop("'class1' must be the name of a class or a class definition")
    if(missing(class2)) {
        if(is.null(classDef1))
            return(class1)
        ext <- classDef1@contains
        if(!isTRUE(maybe) && length(ext) > 0)
        {
            noTest <- vapply(ext, function(obj)isTRUE(body(obj@test)), NA)
            ext <- ext[noTest]
        }
        if(fullInfo) {
            ext[[class1]] <- TRUE
            return(ext)
        }
        else
            return(c(class1,names(ext)))
    }
    value <- NULL
    if(is.character(class2) && length(class2) == 1L) { ## fast first checks
	## the [[1L]] below handles old-style classes & throws away package attributes
	if(.identC(class1[[1L]], class2) || .identC(class2, "ANY"))
          return(TRUE)
        if(!is.null(classDef1) && class2 %in% names(classDef1@contains))
	    value <- classDef1@contains[[class2]]
        else
          classDef2 <- getClassDef(class2)
    }
    else if(is(class2, "classRepresentation")) {
	classDef2 <- class2
	class2 <- class2@className
    }
    else
	stop("'class2' must be the name of a class or a class definition")
    if(is.null(value))
      value <- possibleExtends(class1, class2, classDef1, classDef2)
    if(fullInfo)
        value
    else if(is.logical(value))
        value
    else if(value@simple || isTRUE(body(value@test)))
        TRUE
    else
        maybe
}

.specialVirtual <- c("oldClass")

setIs <-
  ## Defines class1 to be an extension of class2.
  ## The relationship can be conditional, if a function is supplied as the `test'
  ## argument.  If a function is supplied as the `coerce' argument, this function will
  ## be applied to any `class1' object in order to turn it into a `class2' object.
  ##
  ## Extension may imply that a `class1' object contains a `class2' object.  The default
  ## sense of containing is that all the slots of the simpler class are found in the
  ## more elaborate one.  If the `replace' argument is supplied as an S replacement
  ## function, this function will be used to implement `as(obj, class2) <- value'.
  function(class1, class2, test = NULL, coerce = NULL,
           replace = NULL, by = character(), where = topenv(parent.frame()),
           classDef = getClass(class1, TRUE, where = where), extensionObject = NULL, doComplete = TRUE)
{
    ## class2 should exist
    where <- as.environment(where)
    classDef2 <- getClassDef(class2, where)
    if(is.null(classDef2))
        stop(gettextf("class %s has no visible definition from package or environment %s",
                      dQuote(class2),
                      sQuote(getPackageName(where))),
             domain = NA)
    ## check some requirements:
    ## One of the classes must be on the target environment (so that the relation can
    ## be retained by saving the corresponding image)
    m1 <- classMetaName(class1)
    local1 <- exists(m1, where, inherits = FALSE) &&
	!(classDef@sealed || bindingIsLocked(m1, where))
    if(!local1) {
	m2 <- classMetaName(class2)
	local2 <- exists(m2, where, inherits = FALSE) &&
	    !(classDef2@sealed || bindingIsLocked(m2, where))
	if(!local2)
	    stop(gettextf(
		"cannot create a 'setIs' relation when neither of the classes (%s and %s) is local and modifiable in this package",
			dQuote(class1),
			dQuote(class2)),
		 domain = NA)
    }
    if(classDef@sealed && !isClassUnion(classDef2))
        stop(gettextf("class %s is sealed; new superclasses can not be defined, except by 'setClassUnion'",
                      dQuote(class1)),
             domain = NA)
    prevIs <- !identical(possibleExtends(class1, class2,classDef, classDef2),
                         FALSE) # used in checking for previous coerce
    if(is.null(extensionObject))
        obj <- makeExtends(class1, class2, coerce, test, replace, by,
                           classDef1 = classDef, classDef2 = classDef2,
                           package = getPackageName(where))
    else
        obj <- extensionObject
    ## revise the superclass/subclass info in the stored class definition
    ok <- .validExtends(class1, class2, classDef,  classDef2, obj@simple)
    if(!isTRUE(ok))
      stop(ok)
    where2 <- .findOrCopyClass(class2, classDef2, where, "subclass")
    classDef2@subclasses[[class1]] <- obj
    if(doComplete)
        classDef2@subclasses <- completeSubclasses(classDef2, class1, obj, where)
    ## try to provide a valid prototype for virtual classes
    if(classDef2@virtual && is.na(match(class2, .specialVirtual))) {
        ## For simplicity, we prefer NULL prototype if "NULL"
        ## is a subclass of a virtual class; otherwise the
        ## prototype is an element of class1 or its prototype if VIRTUAL
        if(extends(classDef, "NULL"))
            classDef2@prototype <- NULL
        else if(is.null(classDef2@prototype)
                && is.na(match("NULL", names(classDef2@subclasses)))) {
            if(classDef@virtual)
                classDef2@prototype <- classDef@prototype
            else # new(), but without intialize(), which may require an arg.
                classDef2@prototype <- .Call(C_new_object, classDef)
        }
    }
    assignClassDef(class2, classDef2, where2, TRUE)
    .removePreviousCoerce(class1, class2, where, prevIs)
    where1 <- .findOrCopyClass(class1, classDef, where, "superClass")
    ## insert the direct contains information in a valid spot
    .newDirectSuperclass(classDef@contains, class2, names(classDef2@contains)) <- obj
    if(doComplete) {
      classDef@contains <- completeExtends(classDef, class2, obj, where = where)
      if(!is(classDef, "ClassUnionRepresentation")) #unions are handled in assignClassDef
        .checkSubclasses(class1, classDef, class2, classDef2, where1, where2)
    }
    assignClassDef(class1, classDef, where1, TRUE)
    invisible(classDef)
 }

.findOrCopyClass <- function(class, classDef, where, purpose) {
    whereIs <- findClass(classDef, where)
    if(length(whereIs))
      whereIs[[1L]]
    else {
        if(purpose != "subclass")
            warning(gettextf("class %s is defined (with package slot %s) but no metadata object found to revise %s information---not exported?  Making a copy in package %s",
                         .dQ(class), sQuote(classDef@package), purpose,
                         sQuote(getPackageName(where, FALSE))),
                call. = FALSE, domain = NA)
        where
    }
}


.validExtends <- function(class1, class2, classDef1,  classDef2, slotTests) {
    .msg <- function(class1, class2)
        gettextf("class %s cannot extend class %s",
                 dQuote(class1),
                 dQuote(class2))
    if((is.null(classDef1) || is.null(classDef2)) &&
       !(isVirtualClass(class1) && isVirtualClass(class2)))
        return(c(.msg(class1, class2), ": ",
             gettext("both classes must be defined")))
    if(slotTests) {
        slots2 <- classDef2@slots
        if(length(slots2)) {
            n2 <- names(slots2)
            slots1 <- classDef1@slots
            n1 <- names(slots1)
            if(any(is.na(match(n2, n1))))
                return(c(.msg(class1, class2), ": ",
                         sprintf(ngettext(sum(is.na(match(n2, n1))),
                                          "class %s is missing slot from class %s (%s), and no coerce method was supplied",
                                          "class %s is missing slots from class %s (%s), and no coerce method was supplied"),
                                 dQuote(class1),
                                 dQuote(class2),
                                 paste(n2[is.na(match(n2, n1))], collapse = ", "))))
            bad <- character()
            for(what in n2)
                if(!extends(slots1[[what]], slots2[[what]]))
                    bad <- c(bad, what)
            if(length(bad))
                return(c(.msg(class1, class2), ": ",
                         sprintf(ngettext(length(bad),
                                          "slot in class %s must extend corresponding slot in class %s: fails for %s",
                                          "slots in class %s must extend corresponding slots in class %s: fails for %s"),
                                 dQuote(class1),
                                 dQuote(class2),
                                 paste(bad, collapse = ", "))))
        }
    }
    TRUE
}

".newDirectSuperclass<-" <- function(contains, class2, superclasses2, value) {
    superclasses <- names(contains)
    if(length(superclasses2) == 0 || length(superclasses) == 0 ||
       all(is.na(match(superclasses2, superclasses))))
      contains[[class2]] <- value
    else {
        sq <- seq_along(superclasses)
        before <- (sq[match(superclasses, superclasses2,0)>0])[[1]]
        contains <- c(contains[sq < before], value, contains[sq >= before])
        superclasses <- c(superclasses[sq < before], class2, superclasses[sq >= before])
        names(contains) <- superclasses
    }
    contains
}

