% File src/library/methods/man/setMethod.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{setMethod}
\alias{setMethod}
\title{ Create and Save a Method }
\description{
Create a method for a generic function, corresponding to a signature of classes for the arguments. Standard usage will be of the form:

\code{setMethod(f, signature, definition)}

where \code{f} is the name of the function, \code{signature} specifies the argument classes for which the method applies and \code{definition} is the function definition for the method. 
}
\usage{
setMethod(f, signature=character(), definition,
          where = topenv(parent.frame()),
          valueClass = NULL, sealed = FALSE)
}
\arguments{
  \item{f}{ The character-string name of the generic function. The unquoted name usually works as well (evaluating to the generic function), except for a few functions in the base package.}
  \item{signature}{ The classes required for some of the arguments. Most applications just require one or two character strings matching the first argument(s) in the signature. More complicated cases follow R's rule for argument matching. See the details below; however, if the signature is not trivial, you should use \code{\link{method.skeleton}} to generate a valid call to \code{setMethod}.}
  \item{definition}{ A function definition, which will become the method
    called when the arguments in a call to \code{f} match the
    classes in \code{signature}, directly or through inheritance.
    The definition must be a function with the same formal arguments
    as the generic; however, \code{setMethod()} will handle methods
    that add arguments, if \code{\dots} is a formal argument to the generic.
  See the Details section.
  }
  \item{where, valueClass, sealed}{\emph{These arguments are allowed
        but either obsolete or rarely appropriate.}

      \code{where}: where to store the definition; should be the
      default, the namespace for the package.

  \code{valueClass}{ Obsolete. }

  \code{sealed}{ prevents the method being redefined, but should never
    be needed when the method is defined in the source code of a
    package.}
}
}
\value{
  The function exists for its side-effect. The definition will be stored in a special metadata object and incorporated in the generic function when the corresponding package is loaded into an R session. 
  
}
\details{
The call to \code{setMethod} stores the supplied method definition  in
the metadata table for this generic function in the environment,
typically the global environment or the namespace of a package.
In the case of a package, the table object becomes part of the namespace or environment of the
package.
When the package is loaded into a later session, the
methods will be merged into the table of methods in the corresponding
generic function object.

Generic functions are referenced by the combination of the function name and
the package name;
for example, the function \code{"show"} from the package
\code{"methods"}.
Metadata for methods is identified by the two strings; in particular, the
generic function object itself has slots containing its name and its
package name.
The package name of a generic is set according to the package
from which it originally comes; in particular, and frequently, the
package where a non-generic version of the function originated.
For example, generic functions for all the functions in package \pkg{base} will
have \code{"base"} as the package name, although none of them is an
S4 generic on that package.
These include most of the base functions that are primitives, rather than
true functions; see the section on primitive functions in the
documentation for \code{\link{setGeneric}} for details.

Multiple packages can have methods for the same generic function; that
is, for the same combination of generic function name and package
name.
Even though the methods are stored in separate tables in separate
environments, loading the corresponding packages adds the methods to
the table in the generic function itself, for the duration of the session.

 The class
  names in the signature can be any formal class, including basic
  classes such as \code{"numeric"}, \code{"character"}, and
  \code{"matrix"}.  Two additional special class names can appear:
  \code{"ANY"}, meaning that this argument can have any class at all;
  and \code{"missing"}, meaning that this argument \emph{must not}
  appear in the call in order to match this signature.  Don't confuse
  these two:  if an argument isn't mentioned in a signature, it
  corresponds implicitly to class \code{"ANY"}, not to
  \code{"missing"}.  See the example below.  Old-style (\sQuote{S3})
  classes can also be used, if you need compatibility with these, but
  you should definitely declare these classes by calling
  \code{\link{setOldClass}} if you want S3-style inheritance to work.


  Method definitions can
  have default expressions for arguments, but only if
  the generic function must have \emph{some} default expression for the
  same argument. (This restriction is imposed by the way \R manages
  formal arguments.)
  If so, and if the corresponding argument is
  missing in the call to the generic function, the default expression
  in the method is used.  If the method definition has no default for
  the argument, then the expression supplied in the definition of the
  generic function itself is used, but note that this expression will
  be evaluated using the enclosing environment of the method, not of
  the generic function.
  Method selection does
  not evaluate default expressions.
  All actual (non-missing) arguments in the signature of the
  generic function will be evaluated when a method is selected---when
  the call to \code{standardGeneric(f)} occurs.
  Note that specifying class \code{"missing"} in the signature
  does not require any default expressions.

  It is possible to have some differences between the
  formal arguments to a method supplied to \code{setMethod} and those
  of the generic. Roughly, if the generic has \dots as one of its
  arguments, then the method may have extra formal arguments, which
  will be matched from the arguments matching \dots in the call to
  \code{f}.  (What actually happens is that a local function is
  created inside the method, with the modified formal arguments, and the method
  is re-defined to call that local function.)

  Method dispatch tries to match the class of the actual arguments in a
  call to the available methods collected for \code{f}.  If there is a
  method defined for the exact same classes as in this call, that
  method is used.  Otherwise, all possible signatures are considered
  corresponding to the actual classes or to superclasses of the actual
  classes (including \code{"ANY"}).
  The method having the least distance from the actual classes is
  chosen; if more than one method has minimal distance, one is chosen
  (the lexicographically first in terms of superclasses) but a warning
  is issued.
  All inherited methods chosen are stored in another table, so that
  the inheritance calculations only need to be done once per session
  per sequence of actual classes.
  See
  \link{Methods_Details} and Section 10.7 of the reference for more details.

}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\examples{
\dontshow{  require(stats)
  setClass("track", slots = c(x="numeric", y = "numeric"))
  setClass("trackCurve", contains = "track", slots = c(smooth = "numeric"))
  setClass("trackMultiCurve", slots = c(x="numeric", y="matrix", smooth="matrix"),
          prototype = list(x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))
}

require(graphics)
## methods for plotting track objects (see the example for \link{setClass})
##
## First, with only one object as argument:
setMethod("plot", signature(x="track", y="missing"),
  function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...)
)
## Second, plot the data from the track on the y-axis against anything
## as the x data.
setMethod("plot", signature(y = "track"),
 function(x, y, ...) plot(x, slot(y, "y"), ...)
)
## and similarly with the track on the x-axis (using the short form of
## specification for signatures)
setMethod("plot", "track",
 function(x, y, ...) plot(slot(x, "y"), y,  ...)
)
t1 <- new("track", x=1:20, y=(1:20)^2)
tc1 <- new("trackCurve", t1)
slot(tc1, "smooth") <- smooth.spline(slot(tc1, "x"), slot(tc1, "y"))$y #$
plot(t1)
plot(qnorm(ppoints(20)), t1)
## An example of inherited methods, and of conforming method arguments
## (note the dotCurve argument in the method, which will be pulled out
## of ... in the generic.
setMethod("plot", c("trackCurve", "missing"),
function(x, y, dotCurve = FALSE, ...) {
  plot(as(x, "track"))
  if(length(slot(x, "smooth") > 0))
    lines(slot(x, "x"), slot(x, "smooth"),
         lty = if(dotCurve) 2 else 1)
  }
)
## the plot of tc1 alone has an added curve; other uses of tc1
## are treated as if it were a "track" object.
plot(tc1, dotCurve = TRUE)
plot(qnorm(ppoints(20)), tc1)

## defining methods for a special function.
## Although "[" and "length" are not ordinary functions
## methods can be defined for them.
setMethod("[", "track",
  function(x, i, j, ..., drop) {
    x@x <- x@x[i]; x@y <- x@y[i]
    x
  })
plot(t1[1:15])

setMethod("length", "track", function(x)length(x@y))
length(t1)

## methods can be defined for missing arguments as well
setGeneric("summary") ## make the function into a generic

## A method for summary()
## The method definition can include the arguments, but
## if they're omitted, class "missing" is assumed.

setMethod("summary", "missing", function() "<No Object>")

\dontshow{% <--------------------> all the following are "assertion" tests

stopifnot(identical(summary(), "<No Object>"))

removeMethods("summary")

## for the primitives
## inherited methods

length(tc1)
tc1[-1]

## make sure old-style methods still work.
t11 <- t1[1:15]
identical(t1@y[1:15], t11@y)

## S3 methods, with nextMethod
form <- y ~ x
form[1]

## S3 arithmetic methods
ISOdate(1990, 12, 1)- ISOdate(1980, 12, 1)

## group methods

setMethod("Arith", c("track", "numeric"), function(e1, e2){e1@y <-
  callGeneric(e1@y , e2); e1})

t1  - 100.
t1/2

## check it hasn't screwed up S3 methods
ISOdate(1990, 12, 1)- ISOdate(1980, 12, 1)

## test the .Generic mechanism

setMethod("Compare", signature("track", "track"),
  function(e1,e2) {
  switch(.Generic,
   "==" = e1@y == e2@y,
  NA)
 })

#stopifnot(all(t1==t1))
#stopifnot(identical(t1<t1, NA))


## A test of nested calls to "[" with matrix-style arguments
## applied to data.frames (S3 methods)

setMethod("[", c("trackMultiCurve", "numeric", "numeric"), function(x, i, j, ..., drop) {
### FIXME:  a better version has only 1st arg in signature
### and uses callNextMethod, when this works with primitives.
    x@y <- x@y[i, j, drop=FALSE]
    x@x <- x@x[i]
    x
})


"testFunc" <-
function(cur) {
    sorted <- cur[order(cur[,1]),]
    sorted[ !is.na(sorted[,1]), ]
}

Nrow <- 1000 # at one time, values this large triggered a bug in gc/protect
## the loop here was a trigger for the bug
Niter <- 10
for(i in 1:Niter)  {
    yy <- matrix(stats::rnorm(10*Nrow), 10, Nrow)
    tDF <- as.data.frame(yy)
    testFunc(tDF)
}


tMC <- new("trackMultiCurve", x=seq_len(Nrow), y = yy)
## not enough functions have methods for this class to use testFunc

stopifnot(identical(tMC[1:10, 1:10]@y, yy[1:10, 1:10]))


## verify we can remove methods and generic

removeMethods("-")
removeMethod("length", "track")
removeMethods("Arith")
removeMethods("Compare")

## repeat the test one more time on the primitives

length(ISOdate(1990, 12, 1)- ISOdate(1980, 12, 1))

removeMethods("length")

## methods for \%*\%, which isn't done by the same C code as other ops

setClass("foo", slots = c(m="matrix"))
m1 <- matrix(1:12,3,4)
f1 = new("foo", m=m1)
f2 = new("foo", m=t(m1))

setMethod("\%*\%", c("foo", "foo"),
          function(x,y) callGeneric(x@m, y@m))

stopifnot(identical(f1\%*\%f2, m1\%*\% t(m1)))

removeMethods("\%*\%")
removeMethods("plot")

if(FALSE) ## Hold until removeMethods revised:
  stopifnot(existsFunction("plot", FALSE) && !isGeneric("plot", 1))

## methods for plotData
plotData <- function(x, y, ...) plot(x, y, ...)

setGeneric("plotData")

setMethod("plotData", signature(x="track", y="missing"),
          function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...))

## and now remove the whole generic
removeGeneric("plotData")

stopifnot(!exists("plotData", 1))

##  Tests of method inheritance & multiple dispatch
setClass("A0", slots = c(a0 = "numeric"))
setClass("A1", contains = "A0", slots = c(a1 = "character"))

setClass("B0", slots = c(b0 = "numeric"))
setClass("B1", "B0") # (meaning 'contains = *')
setClass("B2", contains = "B1", slots = c(b2 = "logical"))

setClass("AB0", contains = c("A1", "B2"), slots = c(ab0 = "matrix"))

f1 <- function(x,  y)"ANY"

setGeneric("f1")

setMethod("f1", c("A0", "B1"), function(x, y)"A0 B1")
setMethod("f1", c("B1", "A0"), function(x, y)"B1 A0")

a0 <- new("A0")
a1 <- new("A1")
b0 <- new("B0")
b1 <- new("B1")
b2 <- new("B2")

deparseText <- function(expr)
    paste(deparse(expr), collapse = "\\  ")

mustEqual <- function(e1, e2){
    if(!identical(e1, e2))
        stop(paste("!identical(", deparseText(substitute(e1)),
                   ", ", deparseText(substitute(e2)), ")", sep=""))
}

mustEqual(f1(a0, b0), "ANY")
mustEqual(f1(a1,b0), "ANY")
mustEqual(f1(a1,b1), "A0 B1")
mustEqual(f1(b1,a1), "B1 A0")
mustEqual(f1(b1,b1), "ANY")

## remove classes:  order matters so as not to undefine earlier classes
for(.cl in c("AB0", "A1", "A0", "B2", "B1", "B0"))
    removeClass(.cl)

removeGeneric("f1")

## test of nonstandard generic definition

setGeneric("doubleAnything", function(x) {
  methodValue <- standardGeneric("doubleAnything")
  c(methodValue, methodValue)
})

setMethod("doubleAnything", "ANY", function(x)x)

setMethod("doubleAnything", "character",
          function(x) paste("<",x,">",sep=""))

mustEqual(doubleAnything(1:10), c(1:10, 1:10))
mustEqual(doubleAnything("junk"), rep("<junk>",2))

removeGeneric("doubleAnything")
}% dontshow the above
}

\seealso{

\link{Methods_for_Nongenerics} discusses method definition for
functions that are not generic functions in their original package;
\link{Methods_for_S3} discusses the integration of formal methods with the
older S3 methods.

\code{\link{method.skeleton}}, which is the recommended way to generate a skeleton of the call to \code{setMethod}, with the correct formal arguments and other details.

\link{Methods_Details} and the links there for a general discussion, \code{\link{dotsMethods}} for methods that dispatch on
  \dQuote{\dots}, and \code{\link{setGeneric}} for generic functions.
}

\keyword{programming}
\keyword{classes}
\keyword{methods}
