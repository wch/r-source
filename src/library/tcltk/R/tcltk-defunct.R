## deprecated at r16598, 2001-11-04
"$.tclvar" <- function(x, name)
	.Defunct("tclVar and tclvalue")

"$<-.tclvar" <- function(x, name, value)
    .Defunct("tclVar and tclvalue<-")

## deprecated in R 2.3.0
tkcmd <- function(...)
    .Defunct("tcl")

tkfile.tail <- function(...)
    .Defunct("tclfile.tail")

tkfile.dir <- function(...)
    .Defunct("tclfile.dir")

tkopen <- function(...)
    .Defunct("tclopen")

tkclose <- function(...)
    .Defunct("tclclose")

tkputs <- function(...)
    .Defunct("tclputs")

tkread <- function(...)
    .Defunct("tclread")

