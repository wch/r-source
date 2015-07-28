#  File src/library/methods/R/fixPrevious.R
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

## fixPre1.8(names)
##   The objects in names should have been loaded from a version of R
##   previous to 1.8.0
##   The classes of these objects must be defined in the current session.
##   Objects are modified to have the correct version of its class,
##   and re-assigned.
##
##   The function checks for existence and consistency of class definitions.
fixPre1.8 <- function(names, where = topenv(parent.frame())) {
    done <- character()
    for(what in names) {
        objWhere <- .findAll(what, where)
        if(length(objWhere) == 0) {
            warning(gettextf("object %s not found",
                             sQuote(what)),
                    domain = NA)
            next
        }
        objWhere <- objWhere[[1L]]
        obj <- get(what, objWhere)
        ## don't fix up basic datatypes with no explicit class
        if(is.null(attr(obj, "class")))
            next
        Class <- class(obj)
        if(is.null(attr(Class, "package"))) {
            if(isClass(Class, where = where)) {
                ClassDef <- getClass(Class, where = where)
                ok <- !(isVirtualClass(ClassDef) ||
			!isTRUE(validObject(obj, test=TRUE)))
                if(ok) {
                    class(obj) <- ClassDef@className
                    assign(what, obj, objWhere)
                    done <- c(done, what)
                }
                else
                    warning(gettextf("object %s not changed (it is not consistent with the current definition of class %s from %s)",
                                     sQuote(what),
                                     dQuote(Class),
                                     sQuote(ClassDef@package)),
                            domain = NA)
            }
            else
                warning(gettextf("no definition for the class of %s (class %s) found",
                                 sQuote(what),
                                 dQuote(class)),
                        domain = NA)
        }
        else
            warning(gettextf("object %s not changed (it does not appear to be from a version of R earlier than 1.8.0)",
                             sQuote(what)),
                    domain = NA)
    }
    done
}


