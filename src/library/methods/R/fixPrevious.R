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
        objWhere <- methods:::.findAll(what, where)
        if(length(objWhere) == 0) {
            warning(gettextf("object '%s' not found", what), domain = NA)
            next
        }
        objWhere <- objWhere[[1]]
        obj <- get(what, objWhere)
        ## don't fix up basic datatypes with no explicit class
        if(is.null(attr(obj, "class")))
            next
        Class <- class(obj)
        if(is.null(attr(Class, "package"))) {
            if(isClass(Class, where = where)) {
                ClassDef <- getClass(Class, where = where)
                ok <- !(isVirtualClass(ClassDef) ||
                        is(trySilent(validObject(obj)), "try-error"))
                if(ok) {
                    class(obj) <- ClassDef@className
                    assign(what, obj, objWhere)
                    done <- c(done, what)
                }
                else
                    warning(gettextf("object '%s' not changed (it is not consistent with the current definition of class \"%s\" from '%s')", what, Class, ClassDef@package), domain = NA)
            }
            else
                warning(gettextf("no definition for the class of '%s' (class \"%s\") found", what, class), dpmain = NA)
        }
        else
            warning(gettextf("object '%s' not changed (it does not appear to be from a version of R earlier than 1.8.0)", what), domain = NA)
    }
    done
}


