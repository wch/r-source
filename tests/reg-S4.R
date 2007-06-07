##--- S4 Methods (and Classes)
library(methods)
##too fragile: showMethods(where = "package:methods")

##-- S4 classes with S3 slots [moved from ./reg-tests-1.R]
setClass("test1", representation(date="POSIXct"))
x <- new("test1", date=as.POSIXct("2003-10-09"))
stopifnot(format(x @ date) == "2003-10-09")
## line 2 failed in 1.8.0 because of an extraneous space in "%in%"

stopifnot(all.equal(3:3, 3.), all.equal(1., 1:1))

## trace (requiring methods):
f <- function(x, y) { c(x,y)}
xy <- 0
trace(f, quote(x <- c(1, x)), exit = quote(xy <<- x), print = FALSE)
fxy <- f(2,3)
stopifnot(identical(fxy, c(1,2,3)))
stopifnot(identical(xy, c(1,2)))
untrace(f)

## a generic and its methods

setGeneric("f")
setMethod("f", c("character", "character"), function(x,	 y) paste(x,y))

## trace the generic
trace("f", quote(x <- c("A", x)), exit = quote(xy <<- c(x, "Z")), print = FALSE)

## should work for any method

stopifnot(identical(f(4,5), c("A",4,5)),
          identical(xy, c("A", 4, "Z")))

stopifnot(identical(f("B", "C"), paste(c("A","B"), "C")),
          identical(xy, c("A", "B", "Z")))

## trace a method
trace("f", sig = c("character", "character"), quote(x <- c(x, "D")),
      exit = quote(xy <<- xyy <<- c(x, "W")), print = FALSE)

stopifnot(identical(f("B", "C"), paste(c("A","B","D"), "C")))
# These two got broken by Luke's lexical scoping fix
#stopifnot(identical(xy, c("A", "B", "D", "W")))
#stopifnot(identical(xy, xyy))

## but the default method is unchanged
stopifnot(identical(f(4,5), c("A",4,5)),
          identical(xy, c("A", 4, "Z")))

removeGeneric("f")
## end of moved from trace.Rd


## print/show dispatch  [moved from  ./reg-tests-2.R ]
## The results  have waffled back and forth.
## Currently (R 2.4.0) the intent is that automatic printing of S4
## objects should correspond to a call to show(), as per the green
## book, p. 332.  Therefore, the show() method is called, once defined,
## for auto-printing foo, regardless of the S3 or S4 print() method.
setClass("bar", representation(a="numeric"))
foo <- new("bar", a=pi)
foo
show(foo)
print(foo)

setMethod("show", "bar", function(object){cat("show method\n")})
show(foo)
foo
print(foo)
# suppressed because output depends on current choice of S4 type or
# not.  Can reinstate when S4 type is obligatory
# print(foo, digits = 4)

print.bar <- function(x, ...) cat("print method\n")
foo
print(foo)
show(foo)

setMethod("print", "bar", function(x, ...){cat("S4 print method\n")})
foo
print(foo)
show(foo)
## calling print() with more than one argument suppresses the show()
## method, largely to prevent an infinite loop if there is in fact no
## show() method for this class.  A better solution would be desirable.
print(foo, digits = 4)

setClassUnion("integer or NULL", members = c("integer","NULL"))
setClass("c1", representation(x = "integer", code = "integer or NULL"))
nc <- new("c1", x = 1:2)
str(nc)# gave ^ANULL^A in 2.0.0
##


library(stats4)
showMethods("coerce", classes=c("matrix", "numeric"))
## {gave wrong result for a while in R 2.4.0}

## the following showMethods() output tends to generate errors in the tests
## whenever the contents of the packages change. Searching in the
## diff's can easily mask real problems.  If there is a point
## to the printout, e.g., to verify that certain methods exist,
## hasMethod() would be a useful replacement

## showMethods(where = "package:stats4")
## showMethods("show")
## showMethods("show")
## showMethods("plot") # (ANY,ANY) and (profile.mle, missing)
## showMethods(classes="mle")
## showMethods(classes="matrix")


##--- "[" fiasco before R 2.2.0 :
d2 <- data.frame(b= I(matrix(1:6,3,2)))
## all is well:
d2[2,]
stopifnot(identical(d2[-1,], d2[2:3,]))
## Now make "[" into S4 generic by defining a trivial method
setClass("Mat", representation(Dim = "integer", "VIRTUAL"))
setMethod("[", signature(x = "Mat",
			 i = "missing", j = "missing", drop = "ANY"),
	  function (x, i, j, drop) x)
## Can even remove the method: it doesn't help
removeMethod("[", signature(x = "Mat",
                            i = "missing", j = "missing", drop = "ANY"))
d2[1:2,] ## used to fail badly; now okay
stopifnot(identical(d2[-1,], d2[2:3,]))
## failed in R <= 2.1.x


## Fritz' S4 "odditiy"
setClass("X", representation(bar="numeric"))
setClass("Y", contains="X")
## Now we define a generic foo() and two different methods for "X" and
## "Y" objects for arg missing:
setGeneric("foo", function(object, arg) standardGeneric("foo"))
setMethod("foo", signature(object= "X", arg="missing"),
          function(object, arg) cat("an X object with bar =", object@bar, "\n"))
setMethod("foo", signature(object= "Y", arg="missing"),
          function(object, arg) cat("a Y object with bar =", object@bar, "\n"))
## Finally we create a method where arg is "logical" only for class
## "X", hence class "Y" should inherit that:
setMethod("foo", signature(object= "X", arg= "logical"),
          function(object, arg) cat("Hello World!\n") )
## now create objects and call methods:
y <- new("Y", bar=2)
## showMethods("foo")
foo(y)
foo(y, arg=TRUE)## Hello World!
## OK, inheritance worked, and we have
## showMethods("foo")
foo(y)
## still 'Y' -- was 'X object' in R < 2.3


## Multiple inheritance
setClass("A", representation(x = "numeric"))
setClass("B", representation(y = "character"))
setClass("C", contains = c("A", "B"), representation(z = "logical"))
new("C")
setClass("C", contains = c("A", "B"), representation(z = "logical"),
         prototype = prototype(x = 1.5, y = "test", z = TRUE))
(cc <- new("C"))
## failed reconcilePropertiesAndPrototype(..) after svn r37018

## "Logic" group -- was missing in R <= 2.4.0
stopifnot(all(getGroupMembers("Logic") %in% c("&", "|")),
	  any(getGroupMembers("Ops") == "Logic"))
setClass("brob", contains="numeric")
b <- new("brob", 3.14)
logic.brob.error <- function(nm)
    stop("logic operator '", nm, "' not applicable to brobs")
logic2 <- function(e1,e2) logic.brob.error(.Generic)
setMethod("Logic", signature("brob", "ANY"), logic2)
setMethod("Logic", signature("ANY", "brob"), logic2)
## Now ensure that using group members gives error:
assertError <- function(expr)
    stopifnot(inherits(try(expr, silent = TRUE), "try-error"))
assertError(b & b)
assertError(b | 1)
assertError(TRUE & b)

## methods' hidden cbind() / rbind:
cBind <- methods:::cbind
setClass("myMat", representation(x = "numeric"))
setMethod("cbind2", signature(x = "myMat", y = "missing"), function(x,y) x)
m <- new("myMat", x = c(1, pi))
stopifnot(identical(m, cBind(m)))


## explicit print or show on a basic class with an S4 bit
## caused infinite recursion
setClass("Foo", representation(name="character"), contains="matrix")
(f <- new("Foo", name="Sam", matrix()))
(m <- as(f, "matrix"))
show(m)
print(m)
## fixed in 2.5.0 patched

## callGeneric inside a method with new arguments {hence using .local()}:
setGeneric("Gfun", function(x, ...) standardGeneric("Gfun"),
	   useAsDefault = function(x, ...) sum(x, ...))
setClass("myMat", contains="matrix")
setClass("mmat2", contains="matrix")
setClass("mmat3", contains="mmat2")
setMethod(Gfun, signature(x = "myMat"),
	  function(x, extrarg = TRUE) {
	      cat("in 'myMat' method for 'Gfun() : extrarg=", extrarg, "\n")
	      Gfun(unclass(x))
	  })
setMethod(Gfun, signature(x = "mmat2"),
	  function(x, extrarg = TRUE) {
	      cat("in 'mmat2' method for 'Gfun() : extrarg=", extrarg, "\n")
	      x <- unclass(x)
	      callGeneric()
	  })
setMethod(Gfun, signature(x = "mmat3"),
	  function(x, extrarg = TRUE) {
	      cat("in 'mmat3' method for 'Gfun() : extrarg=", extrarg, "\n")
	      x <- as(x, "mmat2")
	      callGeneric()
	  })
(mm <- new("myMat", diag(3)))
Gfun(mm)
Gfun(mm, extrarg = FALSE)
m2 <- new("mmat2", diag(3))
Gfun(m2)
Gfun(m2, extrarg = FALSE)
## The last two gave Error ...... variable ".local" was not found
(m3 <- new("mmat3", diag(3)))
Gfun(m3)
Gfun(m3, extrarg = FALSE) # used to not pass 'extrarg'

