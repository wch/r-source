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

setOldClass <- function(Classes, prototype,
                        where = topenv(parent.frame()), test = FALSE) {
    if(test)
        return(.setOldIs(Classes, where))
    mainClass <- Classes[[1]]
    prevClass <- "oldClass"
    for(cl in rev(Classes)) {
        if(isClass(cl, where)) {
            if(!extends(cl, prevClass))
                warning(gettextf("inconsistent old-style class information for \"%s\" (maybe mixing old and new classes?)", cl), domain = NA)
        }
        else
            setClass(cl, contains = c(prevClass, "VIRTUAL"), where = where)
        prevClass <- cl
    }
    if(!missing(prototype)) {
      cl <- Classes[[1]]
      prevClass <- if(length(Classes)>1) Classes[[2]] else "oldClass"
      setClass(cl, contains = prevClass, prototype = prototype, where = where)
    }

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
