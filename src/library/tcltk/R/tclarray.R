tclArray <- function() {
    x <- tclVar()
    tcl("unset", x)
    tcl("array", "set", x, "")
    class(x) <- c(class(x), "tclArray")
    x
}

"[[.tclArray" <- function(x, ...) {
    name <- as.character(x)
    i <- paste(...,sep=",")
    rval <- .External("RTcl_GetArrayElem", name, i, PACKAGE = "tcltk")
    if (!is.null(rval)) class(rval)<-"tclObj"
    rval
}

"[[<-.tclArray" <- function(x, ..., value){
    name <- as.character(x)
    i <- paste(..., sep=",")
    if (is.null(value))
        .External("RTcl_RemoveArrayElem", name, i, PACKAGE = "tcltk")
    else {
        value <- as.tclObj(value)
        .External("RTcl_SetArrayElem", name, i, value, PACKAGE = "tcltk")
    }
    x
}

"$.tclArray" <- function(x, i) {
    name <- as.character(x)
    i <- as.character(i)
    rval <- .External("RTcl_GetArrayElem", name, i, PACKAGE = "tcltk")
    if (!is.null(rval)) class(rval)<-"tclObj"
    rval
}

"$<-.tclArray" <- function(x, i, value){
    name <- as.character(x)
    i <- as.character(i)
    if (is.null(value))
        .External("RTcl_RemoveArrayElem", name, i, PACKAGE = "tcltk")
    else {
        value <- as.tclObj(value)
        .External("RTcl_SetArrayElem", name, i, value, PACKAGE = "tcltk")
    }
    x
}

names.tclArray <- function(x)
    as.character(tcl("array", "names", x))

"names<-.tclArray" <- function(x, value)
    stop("cannot change names on Tcl array")

length.tclArray <- function(x)
    as.integer(tcl("array", "size", x))

"length<-.tclArray" <- function(x, value)
    stop("cannot set length of Tcl array")



