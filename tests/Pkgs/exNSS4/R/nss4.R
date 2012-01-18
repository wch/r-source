setClass("pubClass", representation("numeric", id = "integer"))

setClass("privCl", representation(x = "numeric", id = "integer"))

.showMe <- function(object)
    cat("show()ing object of class ", class(object),
	" and slots named\n\t",
	paste(slotNames(object), collapse=", "), "\n")

setMethod("show", "pubClass", .showMe)
setMethod("show", "privCl", .showMe)

setMethod("plot", "pubClass", function(x, ...) plot(as(x, "numeric"), ...))
setMethod("plot", "privCl", function(x, ...) plot(x@x, ...))

## this is exported:
assertError <- function(expr)
    stopifnot(inherits(try(expr, silent = TRUE), "try-error"))
## this one is not:
assertWarning <- function(expr)
    stopifnot(inherits(tryCatch(expr, warning = function(w)w), "warning"))

if(isGeneric("colSums")) {
    stop("'colSums' is already generic -- need new example in test ...")
} else {
    setGeneric("colSums")
    stopifnot(isGeneric("colSums"))
}

assertError(setGeneric("pubGenf"))# error: no skeleton

setGeneric("pubGenf", function(x,y) standardGeneric("pubGenf"))

## a private generic {not often making sense}:
setGeneric("myGenf", function(x,y){ standardGeneric("myGenf") })
setMethod("myGenf",  "pubClass", function(x, y) 2*x)

## "(x, ...)" not ok, as generic has no '...':
assertError(setMethod("pubGenf", "pubClass", function(x, ...) { 10*x } ))
## and this is ok
setMethod("pubGenf", c(x="pubClass"), function(x, y) { 10*x } )
