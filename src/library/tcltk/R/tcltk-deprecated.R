## deprecated at r16598, 2001-11-04
"$.tclvar" <- function(x, name) {
	.Deprecated("tclVar and tclvalue")
	.Tcl(paste("set", name))
}

"$<-.tclvar" <- function(x, name, value) {
    .Deprecated("tclVar and tclvalue<-")
    .Tcl(paste("set ", name, " {", value,"}", sep=""))
    x
}

## deprecated in R 2.3.0
tkcmd <- function(...) {
    .Deprecated("tcl")
    .Tcl.objv(.Tcl.args.objv(...))
}
tkfile.tail <- function(...) {
    .Deprecated("tclfile.tail")
    tcl("file", "tail", ...)
}
tkfile.dir <- function(...) {
    .Deprecated("tclfile.dir")
    tcl("file", "dir", ...)
}
tkopen <- function(...) {
    .Deprecated("tclopen")
    tcl("open", ...)
}
tkclose <- function(...) {
    .Deprecated("tclclose")
    tcl("close", ...)
}
tkputs <- function(...) {
    .Deprecated("tclputs")
    tcl("puts", ...)
}
tkread <- function(...) {
    .Deprecated("tclread")
    tcl("read", ...)
}

