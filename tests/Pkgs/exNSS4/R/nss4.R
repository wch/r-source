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

## Long signature in generic; method not specifying all
setGeneric("pubfn",
           function(filename,
                    dimLengths,
                    dimSteps,
                    dimStarts,
                    likeTemplate,
                    likeFile) {
               function(x,y) standardGeneric("pubfn")
           })
setMethod("pubfn", signature=
                       signature(filename="character",
                                 dimLengths="numeric",
                                 dimSteps="numeric",
                                 dimStarts="numeric"),
          function(filename=filename,
                   dimLengths=NULL,
                   dimSteps=NULL, dimStarts=NULL) {
              sys.call()
          })




### "Same" class as in Matrix (but different 'Extends'!)  {as in Rmpfr}

## "atomic vectors" (-> ?is.atomic ) -- exactly as in "Matrix":
## ---------------
setClassUnion("atomicVector", ## "double" is not needed, and not liked by some
	      members = c("logical", "integer", "numeric",
			  "complex", "raw", "character"))

setClassUnion("array_or_vector",
	      members = c("array", "matrix", "atomicVector"))


## Non trivial class union (in the sense that subclasses are S4)
## derived from a Matrix pkg analogon:
## NB:  exportClasses(..) all these *but* "mM" (!)
setClass("M", contains = "VIRTUAL",
	 slots = c(Dim = "integer", Dimnames = "list"),
	 prototype = prototype(Dim = integer(2), Dimnames = list(NULL,NULL)))
setClass("dM",    contains = c("M", "VIRTUAL"), slots = c(x = "numeric"))
setClass("diagM", contains = c("M", "VIRTUAL"), slots = c(diag = "character"))
setClass("ddiM", contains = c("diagM", "dM"))
## now the class union .. that is *NOT* exported
setClassUnion("mM", members = c("matrix", "M"))
