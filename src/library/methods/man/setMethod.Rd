\name{setMethod}
\alias{setMethod}
\alias{removeMethod}
\title{ Create and Save a Method }
\description{
  Create and save a formal method for a given function and list of classes.
}
\usage{
setMethod(f, signature=character(), definition,
          where = topenv(parent.frame()),
          valueClass = NULL, sealed = FALSE)

removeMethod(f, signature, where)
}
\arguments{
  \item{f}{ A generic function or the character-string name of the function. }
  \item{signature}{ A match of formal argument names for \code{f} with
    the character-string names of corresponding classes.  This
    argument can also just be the vector of class names, in which case
    the first name corresponds to the first formal argument, the
    next to the second formal argument, etc.}
  \item{definition}{ A function definition, which will become the method
    called when the arguments in a call to \code{f} match the
    classes in \code{signature}, directly or through inheritance. }
  \item{where}{the database in which to store the definition of the
    method; % FIXME: by default, the ... environment.

    For \code{removeMethod}, the default is the location of the (first)
    instance of the method for this signature.}
  \item{valueClass}{ If supplied, this argument asserts that the
    method will return a value of this class.  (At present this
    argument is stored but not explicitly used.) }
  \item{sealed}{ If \code{TRUE}, the method so defined cannot be
      redefined by another call to \code{setMethod} (although it can
      be removed and then re-assigned).  Note that this argument is an
      extension to the definition of \code{setMethod} in the reference.}
}
\value{
  These functions exist for their side-effect, in setting or removing a
  method in the object defining methods for the specified generic.

  The value returned by \code{removeMethod} is \code{TRUE} if a method
  was found to be removed.
}
\details{
  R methods for a particular generic function are stored in an object
  of class \code{\link{MethodsList}}.  The effect of calling
  \code{setMethod} is to store \code{definition}  in a \code{MethodsList}
  object on database \code{where}.   If \code{f} doesn't
  exist as a generic function, but there is an ordinary function of
  the same name and the same formal arguments, a new generic function
  is created, and the previous non-generic version of \code{f} becomes
  the default method.  This is equivalent to the programmer calling
  \code{\link{setGeneric}} for the same function; it's better practice
  to do the call explicitly, since it shows that you intend to turn
  \code{f} into a generic function.

  Methods are stored in a hierarchical structure:  see
  \link{Methods} for how the objects are used to select a method, and
  \code{\link{MethodsList}} for functions that manipulate the objects.

 The class
  names in the signature can be any formal class, plus predefined basic
  classes such as \code{"numeric"}, \code{"character"}, and
  \code{"matrix"}.  Two additional special class names can appear:
  \code{"ANY"}, meaning that this argument can have any class at all;
  and \code{"missing"}, meaning that this argument \emph{must not}
  appear in the call in order to match this signature.  Don't confuse
  these two:  if an argument isn't mentioned in a signature, it
  corresponds implicitly to class \code{"ANY"}, not to
  \code{"missing"}.  See the example below.  Old-style (\dQuote{S3})
  classes can also be used, if you need compatibility with these, but
  you should definitely declare these classes by calling
  \code{\link{setOldClass}} if you want S3-style inheritance to work.


  While \code{f} can correspond to methods defined on several packages
  or environments, the underlying model is that these together make up
  the definition for a single generic function.  When R proceeds to
  select and evaluate methods for \code{f}, the methods on the current
  search list are merged to form a single generic function and
  associated methods list.  When \code{f}
  is called and a method is \dQuote{dispatched}, the evaluator matches the
  classes of the actual arguments to the signatures of the available
  methods.  When a match is found, the body of the corresponding
  method is evaluated, but without rematching the arguments to
  \code{f}.   Aside from not rematching the arguments, the computation
  proceeds as if the call had been to the method.  In particular, the
  lexical scope of the method is used.

  Method definitions can
  have default expressions for arguments.  If those arguments are then
  missing in the call to the generic function, the default expression
  in the method is used.  If the method definition has no default for
  the argument, then the expression (if any) supplied in the definition of the
  generic function itself is used.  But note that this expression will
  be evaluated in the environment defined by the method.

  It is possible to have some differences between the
  formal arguments to a method supplied to \code{setMethod} and those
  of the generic. Roughly, if the generic has \dots as one of its
  arguments, then the method may have extra formal arguments, which
  will be matched from the arguments matching \dots in the call to
  \code{f}.  (What actually happens is that a local function is
  created inside the method, with its formal arguments, and the method
  is re-defined to call that local function.)

  Method dispatch tries to match the class of the actual arguments in a
  call to the available methods collected for \code{f}.  Roughly, for
  each formal argument in turn, we look for the best match (the exact
  same class or the nearest element in the value of
  \code{\link{extends}} for that class) for which there is any
  possible method matching the remaining arguments.  See
  \link{Methods} for more details.

}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see \code{?\link{Methods}} and the
  pointers from that documentation..
}
\examples{
\dontshow{  require(stats)
  setClass("track",
    representation(x="numeric", y = "numeric"))
  setClass("trackCurve", representation("track",
    smooth = "numeric"))
  setClass("trackMultiCurve", representation(x="numeric", y="matrix", smooth="matrix"),
          prototype = list(x=numeric(), y=matrix(0,0,0), smooth=
  matrix(0,0,0)))
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

\dontshow{

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

## methods for %*%, which isn't done by the same C code as other ops

setClass("foo", representation(m="matrix"))
m1 <- matrix(1:12,3,4)
f1 = new("foo", m=m1)
f2 = new("foo", m=t(m1))

setMethod("\%*\%", c("foo", "foo"),
 function(x,y)callGeneric(x@m, y@m))

stopifnot(identical(f1\%*\%f2, m1\%*\% t(m1)))

removeMethods("\%*\%")

removeMethods("plot")

stopifnot(existsFunction("plot", FALSE) && !isGeneric("plot", 1))

## methods for plotData
plotData <- function(x, y, ...) plot(x, y, ...)

setGeneric("plotData")

setMethod("plotData", signature(x="track", y="missing"),
  function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...)
)
## and now remove the whole generic
removeGeneric("plotData")

stopifnot(!exists("plotData", 1))

##  Tests of method inheritance & multiple dispatch
setClass("A0", representation(a0 = "numeric"))

setClass("A1", representation("A0", a1 = "character"))

setClass("B0" ,representation(b0 = "numeric"))

setClass("B1", "B0")

setClass("B2", representation("B1", b2 = "logical"))

setClass("AB0", representation("A1", "B2", ab0 = "matrix"))

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
function(x)paste("<",x,">",sep=""))

mustEqual(doubleAnything(1:10), c(1:10, 1:10))
mustEqual(doubleAnything("junk"), rep("<junk>",2))

removeGeneric("doubleAnything")


}
}

\seealso{

\link{method.skeleton}, which is the recommended way to generate a skeleton of the call to \code{setMethod}, with the correct formal arguments and other details.

\link{Methods}, \code{\link{MethodsList}} for details of the
  implementation
}

\keyword{programming}
\keyword{classes}
\keyword{classes}
\keyword{methods}
