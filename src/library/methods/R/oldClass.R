## assumes oldClass has been defined as a virtual class

setOldClass <- function(Classes, where = topenv(parent.frame()), test = FALSE) {
    if(test)
        return(.setOldIs(Classes, where))
    prevClass <- "oldClass"
    for(cl in rev(Classes)) {
        if(isClass(cl, where)) {
            if(!extends(cl, prevClass))
                warning("inconsistent old-style class information for \"",
                        cl,"\" (maybe mixing old and new classes?)")
        }
        else
            setClass(cl, representation(prevClass, "VIRTUAL"), where = where)
        prevClass <- cl
    }
}

.oldTestFun <- function(object) CLASS %in% attr(object, "class")
.oldCoerceFun <- function(from, strict = TRUE) {
    if(strict)
        stop("Explicit coercion of old-style class (", paste(class(from), collapse = ", "),
             ") is not defined")
    from
}
.oldReplaceFun <- function(from, to, value)
    stop(paste("Explicit replacement not defined for as(x, \"", to,
               "\") <- value for old-style class \"", class(from)[1], "\"", sep=""))

.setOldIs <- function(Classes, where) {
    if(length(Classes) != 2)
        stop("Argument Classes must be a vector of two classes; got an argument of length ",
             length(Classes))
    for(cl in Classes) {
        if(isClass(cl, where)) {
            if(!extends(cl, "oldClass"))
                warning("inconsistent old-style class information for \"",
                        cl,"\" (maybe mixing old and new classes?)")
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
