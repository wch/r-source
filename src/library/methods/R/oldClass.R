## assumes oldClass has been defined as a virtual class

setOldClass <- function(Classes, where = 1) {
    prevClass <- "oldClass"
    for(cl in rev(Classes)) {
        if(isClass(cl)) {
            if(!extends(cl, prevClass))
                warning("inconsistent old-style class information for \"",
                        cl,"\" (maybe mixing old and new classes?)")
        }
        else
            setClass(cl, representation(prevClass, "VIRTUAL"), where = where)
        prevClass <- cl
    }
}
