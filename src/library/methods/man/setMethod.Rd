\name{setMethod}
\alias{setMethod}
\title{ Create and Save a Method }
\description{
  Create and save a formal method for a given function and list of classes.
}
\usage{
setMethod(f, signature=character(), def, where=1, valueClass)
}
\arguments{
  \item{f}{ The character-string name of the generic function. }
  \item{signature}{ A match of formal argument names for \code{f} with
      the character-string names of corresponding classes.  This
      argument can also just be the vector of class names, in which case
      the first name corresponds to the first formal argument, the
      next to the second formal argument, etc.}
  \item{def}{ A function definition, which will become the method
      called when the arguments in a call to \code{f} match the
      classes in \code{signature}, directly or through inheritance. }
  \item{where}{ The database in which to store the definition of the
      method; by default, the current global environment. }
  \item{valueClass}{ If supplied, this argument asserts that the
      method will return a value of this class.  (At present this
      argument is stored but not explicitly used.) }
}
\details{
  R methods for a particular generic function are stored in an object
  of class \code{\link{MethodsList}}, which in turn is stored with the
  definition of the generic function.  The effect of calling
  \code{setMethod} is to store \code{def}  in a \code{MethodsList}
  object in a definition of the generic function on database
  \code{where}.   If no such function exists (on that database) one
  will be created, by copying the generic function from where it is
  found in the current search list.  Finally, if \code{f} doesn't
  exist as a generic function, but there is an ordinary function of
  the same name and the same formal arguments, a new generic function
  is created, and the previous non-generic version of \code{f} becomes
  the default method.

  Methods are stored in a hierarchical structure, by formal arguments
  to \code{f}:  see \code{\link{MethodsList}} for details.  The class
  names in the signature can be any formal class, plus predefined basic
  classes such as \code{"numeric"}, \code{"character"}, and
  \code{"matrix"}.  Two additional special class names can appear:
  \code{"ANY"}, meaning that this argument can have any class at all;
  and \code{"missing"}, meaning that this argument \emph{must not}
  appear in the call in order to match this signature.  Don't confuse
  these two:  if an argument isn't mentioned in a signature, it
  corresponds implicitly to class \code{"ANY"}, not to
  \code{"missing"}.  See the example below.

  While \code{f} can correspond to methods defined on several packages
  or databases, the underlying model is that these together make up
  the definition for a single generic function.  When R proceeds to
  select and evaluate methods for \code{f}, the methods on the current
  search list are merged to form a single generic.  In particular, all
  the versions of \code{f} and all the methods must correspond to the
  same formal arguments (including, in the present definition, the
  same default expressions for the arguments).  For compatibility with
  S-Plus, the current implementation enforces this partly with a
  warning and a reconstruction of a method that fails to match, but
  don't count on this for the future:  Make the formal arguments of
  \code{def} match those of the generic..
}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\examples{
\testonly{  library(modreg)
 if(!isClass("track")) {
  setClass("track",
    representation(x="numeric", y = "numeric"))
  setSClass("trackCurve", extends = "track",
    properties = list(smooth = "numeric"))
 }
}
## A function like plot, but with explicit x, y arguments:
plotData <- function(x, y, ...) {
  if(missing(y)) plot(x, ...)
  else plot(x, y, ...)
 }
## methods for plotting track objects (see the example for \link{setClass})
##
## First, with only one object as argument:
setMethod("plotData", signature(x="track", y="missing"),
  function(x,  y, ...) plotData(slot(x, "x"), slot(x, "y"), ...)
)
## Second, plot the data from the track on the y-axis against anything
## as the x data.
setMethod("plotData", signature(y = "track"),
 function(x, y, ...) plotData(x, slot(y, "y"), ...)
)
## and similarly with the track on the x-axis (using the short form of
## specification for signatures)
setMethod("plotData", "track",
 function(x, y, ...) plotData(slot(x, "y"), y,  ...)
)
t1 <- new("track", x=1:30, y=sort(rnorm(30)))
tc1 <- new("trackCurve", t1)
slot(tc1, "smooth") <- smooth.spline(slot(tc1, "x"), slot(tc1, "y"))$y
plotData(t1)
plotData(qnorm(ppoints(30)), t1)
## An example of inherited methods.
setMethod("plotData", c("trackCurve", "missing"),
function(x, y, ...) {
  plotData(as(x, "track"))
  if(length(slot(x, "smooth") > 0))
    lines(slot(x, "x"), slot(x, "smooth"))
  }
)
## the plot of tc1 alone has an added curve; other uses of tc1
## are treated as if it were a "track" object.
plotData(tc1)
plotData(qnorm(ppoints(30)), tc1)
}

\seealso{ \link{Methods}, \code{\link{MethodsList}} }

\keyword{programming}
\keyword{classes}
\keyword{methods}
