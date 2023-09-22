#### Formal classes & methods
#### - Started by moving checks from ./reg-S4.R:
###	all "Matrix" + "late_and_no-relevant-print" now here
#### - No *.Rout.save <==> use stopifnot() etc for testing
#### - Recommended packages allowed, e.g., "Matrix"
if(require("Matrix", lib.loc = .Library, quietly = TRUE)) {
    D5. <- Diagonal(x = 5:1)
    D5N <- D5.; D5N[5,5] <- NA
    stopifnot(isGeneric("dim", where=as.environment("package:Matrix")),
	      identical(D5., pmin(D5.)),
	      identical(D5., pmax(D5.)),
	      identical(D5., pmax(D5., -1)),
	      identical(D5., pmin(D5., 7)),
	      inherits((D5.3 <- pmin(D5.+2, 3)), "Matrix"),
	      identical(as.matrix(pmin(D5.+2 , 3)),
			pmin(as.matrix(D5.+2), 3)),
	      identical(pmin(1, D5.), pmin(1, as.matrix(D5.))),
	      identical(D5N, pmax(D5N, -1)),
	      identical(D5N, pmin(D5N, 5)),
	      identical(as.matrix(pmin(D5N +1, 3)),
			pmin(as.matrix(D5N)+1, 3)),
	      ##
	      TRUE)

  ## R's internal C  R_check_class_and_super()  was not good enough

    setClass("Z", representation(zz = "list"))
    setClass("C", contains = c("Z", "dgCMatrix"))
    setClass("C2", contains = "C")
    setClass("C3", contains = "C2")
    m <- matrix(c(0,0,2:0), 3,5, dimnames = list(NULL,NULL))
    m2 <- matrix(c(0,0,2:0), 3,5)
    show(mC <- as(m, "dgCMatrix"))
    show(cc <- as(mC, "C"))
    c2 <- as(mC, "C2")
    c3 <- as(mC, "C3")
    stopifnot(
        identical(capture.output(c2),
                  sub("C3","C2", capture.output(c3)))
      , identical(as(cc, "matrix"), m2) # changed for Matrix 1.5-x
      , identical(as(c2, "matrix"), m2)
      , identical(as(c3, "matrix"), m2)
    )
    invisible(lapply(c("Z","C","C2","C3"), removeClass))

  ## classUnion <--> class hierarchy :

    sci <- names(getClass("integer")@contains)
    # These 2 classes have *nothing* to do with Matrix:
    setClass("MyClass")
    ncl <- "NumOrMyClass"
    setClassUnion(ncl, c("numeric", "MyClass"))
    nsci <- names(getClass("integer")@contains)
    ## failed in R <= 3.6.2
    stopifnot(sci %in% nsci, identical(setdiff(nsci, sci), ncl))

    setClassUnion('dMatrixOrMatrix', members = c('dMatrix', 'matrix'))
    ## failed in R <= 3.6.2
    stopifnot("dMatrixOrMatrix" %in% names(getClass("dgCMatrix")@contains))

    invisible(lapply(c("NumOrMyClass", "MyClass", "dMatrixOrMatrix"),
                     removeClass))
} else
    message("skipping tests requiring the Matrix package")


## implicit coercion of S4 object to vector via as.vector() in sub-assignment
setClass("A", representation(stuff="numeric"))
as.vector.A <- function (x, mode="any") x@stuff
v <- c(3.5, 0.1)
a <- new("A", stuff=v)
x <- y <- numeric(10)
x[3:4] <- a
y[3:4] <- v
stopifnot(identical(x, y))

## callNextMethod() was broken when augmenting args of primitive generics
foo <- setClass("foo")
bar <- setClass("bar", contains = "foo")

setMethod("[", "foo",  function(x, i, j, ..., flag = FALSE, drop = FALSE) {
    flag
})

setMethod("[", "bar", function(x, i, j, ..., flag = FALSE, drop = FALSE) {
    callNextMethod()
})

BAR <- new("bar")
stopifnot(identical(BAR[1L], FALSE))
stopifnot(identical(BAR[1L, , flag=TRUE], TRUE))

## avoid infinite recursion on Ops,structure methods
setClass("MyInteger",
         representation("integer")
         )
i <- new("MyInteger", 1L)
m <- matrix(rnorm(300), 30,10)
stopifnot(identical(i*m, m))

## when rematching, do not drop arg with NULL default
setGeneric("genericExtraArg",
           function(x, y, extra) standardGeneric("genericExtraArg"),
           signature="x")

setMethod("genericExtraArg", "ANY", function(x, y=NULL) y)

stopifnot(identical(genericExtraArg("foo", 1L), 1L))

## callNextMethod() was broken for ... dispatch
f <- function(...) length(list(...))
setGeneric("f")
setMethod("f", "character", function(...){ callNextMethod() })
stopifnot(identical(f(1, 2, 3), 3L))
stopifnot(identical(f("a", "b", "c"), 3L))

## ... dispatch was evaluating missing arguments in the generic frame
f <- function(x, ..., a = b) {
    b <- "a"
    a
}
setGeneric("f", signature = "...")
stopifnot(identical(f(a=1), 1))
stopifnot(identical(f(), "a"))

## ensure forwarding works correctly for dots dispatch
f2 <- function(...) f(...)
stopifnot(identical(f2(a=1), 1))


## Error messages occurring during method selection are forwarded
f <- function(x) x
setGeneric("f")
setMethod("f", signature("NULL"), function(x) NULL)
err <- tryCatch(f(stop("this is mentioned")), error = identity)
stopifnot(identical(err$message, "error in evaluating the argument 'x' in selecting a method for function 'f': this is mentioned"))


## canCoerce(obj, .)  when length(class(obj)) > 1 :
setOldClass("foo")
setAs("foo", "A", function(from) new("A", foo=from))
o3 <- structure(1:7, class = c("foo", "bar"))
stopifnot( canCoerce(o3, "A") )
## failed in R <= 3.6.1


## Error message of  "unable to find .. method" now lists signature argument names:
setGeneric("BaseGeneric", \(x, y, ...) standardGeneric("BaseGeneric"))
setMethod("BaseGeneric", signature(x = "numeric", y = "numeric"), \(x,y, ...) x + y)
errXY <- try(BaseGeneric(X = 1, Y = 2))
err1  <- try(BaseGeneric(1))
err1Y <- try(BaseGeneric(1, Y = 2))
stopifnot(exprs = {
    identical(3, BaseGeneric(1, 2))
    inherits(errXY, "try-error")
    grepl('x = "missing", y = "missing"', attr(errXY,"condition")$message)
    inherits(err1,  "try-error")
    grepl('x = "numeric", y = "missing"', attr(err1, "condition")$message)
    identical(err1, err1Y) # (as $call is empty)
})
