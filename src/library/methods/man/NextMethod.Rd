% File src/library/methods/man/NextMethod.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{callNextMethod}
\alias{callNextMethod}
\title{Call an Inherited Method}
\description{
  A call to \code{callNextMethod} can only appear inside a method
  definition.  It then results in a call to the first inherited method
  after the current method, with the arguments to the current method
  passed down to the next method.  The value of that method call is the
  value of \code{callNextMethod}.
}
\usage{
callNextMethod(...)
}
\arguments{
  \item{\dots}{
    Optionally, the arguments to the function in its next call
    (but note that the dispatch is as in the detailed description below;
    the arguments have no effect on selecting the next method.)

    If no arguments are included in the call to \code{callNextMethod}, the
    effect is to call the method with the current arguments.
    See the detailed description for what this really means.

    Calling with no arguments is often the natural way to use
    \code{callNextMethod}; see the examples.
    }
}
\details{
  The \sQuote{next} method (i.e., the first inherited method) is defined
  to be that method which \emph{would} have been called if the current
  method did not exist. This is more-or-less literally what happens: The
  current method (to be precise, the method with signature given by the
  \code{defined} slot of the method from which \code{callNextMethod} is
  called) is deleted from a copy of the methods for the current generic,
  and \code{\link{selectMethod}} is called to find the next method (the
  result is cached in the method object where the call occurred, so the search typically
  happens only once per session per combination of argument classes).

  The next method is defined from the \emph{signature} of the current
  method, not from the actual classes of the arguments.
  In particular, modifying any of the arguments has no effect on the
  selection.
  As a result, the selected next method can be called with invalid
  arguments if the calling function assigns objects of a different
  class before the \code{callNextMethod()} call.
  Be careful of any assignments to such arguments.

  It is possible for the selection of the next method to be ambiguous,
  even though the original set of methods was consistent.
  See the section \dQuote{Ambiguous Selection}.

  The statement that the method is called with the current arguments is
  more precisely as follows.  Arguments that were missing in the current
  call are still missing (remember that \code{"missing"} is a valid
  class in a method signature).  For a formal argument, say \code{x}, that
  appears in the original call, there is a corresponding argument in the
  next method call equivalent to \code{x = x}.  In effect, this
  means that the next method sees the same actual arguments, but
  arguments are evaluated only once.
}
\section{Ambiguous Selection}{
There are two fairly common situations in which the choice of a next
method is ambiguous, even when the original set of methods uniquely
defines all method selection unambiguously.
In these situations, \code{callNextMethod()} should be replaced,
either by a call to a specific function or by recalling the generic
with different arguments.

The most likely situation arises with methods for binary operators,
typically through one of the group generic functions.
See the example for class \code{"rnum"} below.
Examples of this sort usually require three methods: two for the case
that the first or the second argument comes from the class, and a
third for the case that both arguments come from the class.
If that last method uses \code{callNextMethod}, the other two methods
are equally valid.  The ambiguity is exactly the same that required
defining the two-argument method in the first place.

In fact, the two possibilities are equally valid conceptually as well
as formally.
As in the example below, the logic of the application usually requires
selecting a computation explicitly or else calling the generic
function with modified arguments to select an appropriate method.

The other likely source of ambiguity arises from a class that inherits
directly from more than one other class (a \dQuote{mixin} in standard
terminology).
If the generic has methods corresponding to both superclasses, a
method for the current class is again needed to resolve ambiguity.
Using \code{callNextMethod} will again reimpose the ambiguity.
Again, some explicit choice has to be made in the calling method
instead.

These ambiguities are not the result of bad design, but they do
require workarounds.
Other ambiguities usually reflect inconsistencies in the tree of
inheritances, such as a class appearing in more than one place among
the superclasses.
Such cases should be rare, but with the independent definition of
classes in multiple packages, they can't be ruled out.

}
\value{
  The value returned by the selected method.
}
\references{
  Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}
\seealso{\code{\link{callGeneric}} to call the generic function with the
 current dispatch rules (typically for a group generic function);
 \link{Methods_Details} for the general behavior of method dispatch.
}

\examples{
## callNextMethod() used for the Math, Math2 group generic functions

## A class to automatically round numeric results to "d" digits

rnum <- setClass("rnum", slots = c(d = "integer"), contains = "numeric")

## Math functions operate on the rounded numbers, return a plain
## vector.  The next method will always be the default, usually a primitive.
setMethod("Math", "rnum",
          function(x)
              callNextMethod(round(as.numeric(x), x@d)))
setMethod("Math2", "rnum",
          function(x, digits)
              callNextMethod(round(as.numeric(x), x@d), digits))

## Examples of callNextMethod with two arguments in the signature.

## For arithmetic and one rnum with anything, callNextMethod with no arguments
## round the full accuracy result, and return as plain vector
setMethod("Arith", c(e1 ="rnum"),
          function(e1, e2)
              as.numeric(round(callNextMethod(), e1@d)))
setMethod("Arith", c(e2 ="rnum"),
          function(e1, e2)
              as.numeric(round(callNextMethod(), e2@d)))

## A method for BOTH arguments from "rnum" would be ambiguous
## for callNextMethod(): the two methods above are equally valid.
## The method chooses the smaller number of digits,
## and then calls the generic function, postponing the method selection
## until it's not ambiguous.
setMethod("Arith", c(e1 ="rnum", e2 = "rnum"),
          function(e1, e2) {
              if(e1@d <= e2@d)
                  callGeneric(e1, as.numeric(e2))
              else
                  callGeneric(as.numeric(e1), e2)
          })

## For comparisons, callNextMethod with the rounded arguments
setMethod("Compare", c(e1 = "rnum"),
          function(e1, e2)
              callNextMethod(round(e1, e1@d), round(e2, e1@d)))
setMethod("Compare", c(e2 = "rnum"),
          function(e1, e2)
              callNextMethod(round(e1, e2@d), round(e2, e2@d)))

## similarly to the Arith case, the method for two "rnum" objects
## can not unambiguously use callNextMethod().  Instead, we rely on
## The rnum() method inhertited from Math2 to return plain vectors.
setMethod("Compare", c(e1 ="rnum", e2 = "rnum"),
          function(e1, e2) {
              d <- min(e1@d, e2@d)
              callGeneric(round(e1, d), round(e2, d))
          })




set.seed(867)

x1 <- rnum(10*runif(5), d=1L)
x2 <- rnum(10*runif(5), d=2L)

x1+1
x2*2
x1-x2

## Simple examples to illustrate callNextMethod with and without arguments
B0 <- setClass("B0", slots = c(s0 = "numeric"))

## and a function to illustrate callNextMethod

f <- function(x, text = "default") {
    str(x) # print a summary
    paste(text, ":", class(x))
}

setGeneric("f")
setMethod("f", "B0", function(x, text = "B0") {
    cat("B0 method called with s0 =", x@s0, "\n")
    callNextMethod()
})

b0 <- B0(s0 = 1)

## call f() with 2 arguments: callNextMethod passes both to the default method
f(b0, "first test")

## call f() with 1 argument:  the default "B0" is not passed by callNextMethod
f(b0)

## Now, a class that extends B0, with no methods for f()
B1 <- setClass("B1", slots = c(s1 = "character"), contains = "B0")
b1 <- B1(s0 = 2, s1 = "Testing B1")

## the two cases work as before, by inheriting the "B0" method

f(b1, b1@s1)

f(b1)

B2 <- setClass("B2", contains = "B1")

## And, a method for "B2" that calls with explicit arguments.
## Note that the method selection in callNextMethod
## uses the class of the *argument* to consistently select the "B0" method

setMethod("f", "B2", function(x, text = "B1 method") {
    y <- B1(s0 = -x@s0, s1 ="Modified x")
    callNextMethod(y, text)
})

b2 <- B2(s1 = "Testing B2", s0 = 10)

f(b2, b2@s1)

f(b2)


## Be careful:  the argument passed must be legal for the method selected
## Although the argument here is numeric, it's still the "B0" method that's called
setMethod("f", "B2", function(x, text = "B1 method") {
    callNextMethod(x@s0, text)
})

##  Now the call will cause an error:

tryCatch(f(b2), error = function(e) cat(e$message,"\n"))


\dontshow{
##$
removeClass("B2"); removeClass("B1"); removeClass("B0")

removeGeneric("f")

removeMethods(all=FALSE,"Arith"); removeMethods(all=FALSE,"Compare")
removeMethods(all=FALSE,"Math"); removeMethods(all=FALSE,"Math2")

## tests of multiple callNextMethod
setClass("m1", slots = c(count = "numeric"), contains = "matrix",
         prototype = prototype(count = 0))
mm1 <- new("m1", matrix(1:12, 3,4))
setMethod("[", "m1", function(x, i, j, ..., drop) callNextMethod())

setClass("m2", slots = c(sum = "numeric"), contains = "m1")

setMethod("Ops", c("m1", "m1"), function(e1, e2) {
    as(e1, "matrix") <- callNextMethod()
    e1@count <- max(e1@count, e2@count)+1
    e1})

mm2 <- new("m2", matrix(1:12, 3, 4), sum = sum(1:12))

stopifnot(identical(mm2[,2], 4:6))

setClass("m3", slots = c(rowtags = "character"),contains = "m2")

setMethod("[", signature(x="m3", i = "character", j = "missing",
                         drop = "missing"),
          function(x, i,j, ..., drop) {
              xx <- callNextMethod(x, match(i, x@rowtags),)
              x@.Data <- xx
              x@rowtags <- x@rowtags[match(i, x@rowtags)]
              x})

tm <- matrix(1:12, 4, 3)

mm3 <- new("m3", tm, rowtags = letters[1:4])

mmm <- mm3[c("b", "d")]

stopifnot(identical(mmm,
      new("m3", tm[c(2, 4),], rowtags = c("b", "d"))))

removeClass("m3")
removeClass("m2")
removeClass("m1")

removeMethods(all=FALSE,"[")
removeMethods(all=FALSE,"Ops")
}

}
\keyword{programming}
\keyword{classes}
\keyword{methods}
