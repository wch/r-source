## assumes oldClass has been defined as a virtual class

setOldClass <- function(Classes, where = topenv(parent.frame()), test = FALSE) {
    if(test)
        return(.setOldIs(Classes, where))
    prevClass <- "oldClass"
    for(cl in rev(Classes)) {
        if(isClass(cl, where)) {
            if(!extends(cl, prevClass))
                warning(gettextf("inconsistent old-style class information for \"%s\" (maybe mixing old and new classes?)", cl), domain = NA)
        }
        else
            setClass(cl, representation(prevClass, "VIRTUAL"), where = where)
        prevClass <- cl
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
