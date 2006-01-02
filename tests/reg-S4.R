##--- S4 Methods (and Classes)
library(methods)
showMethods(where = "package:methods")

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
setClass("bar", representation(a="numeric"))
foo <- new("bar", a=pi)
foo
show(foo)
print(foo)

setMethod("show", "bar", function(object){cat("show method\n")})
show(foo)
foo
print(foo)
print(foo, digits = 4)

print.bar <- function(x, ...) cat("print method\n")
foo
print(foo)
show(foo)

setMethod("print", "bar", function(x, ...){cat("S4 print method\n")})
foo
print(foo)
show(foo)
print(foo, digits = 4)

setClassUnion("integer or NULL", members = c("integer","NULL"))
setClass("c1", representation(x = "integer", code = "integer or NULL"))
nc <- new("c1", x = 1:2)
str(nc)# gave ^ANULL^A in 2.0.0
##


library(stats4)
showMethods(where = "package:stats4")
showMethods("show")
showMethods("show")
showMethods("plot") # (ANY,ANY) and (profile.mle, missing)
showMethods(classes="mle")
showMethods(classes="matrix")
showMethods(classes=c("matrix", "numeric"))
showMethods(where = "package:methods")

## stopifnot(require(Matrix),
##           require(lme4)) # -> S4  plot
## showMethods("plot") # more than last time
## showMethods("show", classes = c("dgeMatrix","Matrix","matrix"))
## showMethods("show")
## showMethods(classes = c("dgeMatrix","matrix"))

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
showMethods("foo")
foo(y)
foo(y, arg=TRUE)## Hello World!
## OK, inheritance worked, and we have
showMethods("foo")
foo(y)
## still 'Y' -- was 'X object' in R < 2.3

