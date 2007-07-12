\name{new}
\alias{new}
\alias{initialize}
\title{ Generate an Object from a Class }
\description{
  Given the name or the definition of a class, plus optionally data
  to be included in the object, \code{new} returns an object from that
  class.
}
\usage{
new(Class, ...)

initialize(.Object, ...)
}
\arguments{
  \item{Class}{ Either the name of a class (the usual case) or the
    object describing the class (e.g., the value returned by
    \code{getClass}).}
  \item{\dots}{ Data to include in the new object.  Named arguments
    correspond to slots in the class definition. Unnamed arguments must
    be objects from classes that this class extends.}
  \item{.Object}{ An object:  see the Details section.}
}
\details{
  The function \code{new} begins by copying the prototype object from
  the class definition.  Then information is inserted according to the
  \code{\dots} arguments, if any.  As of version 2.4 of R, the type of
  the prototype object, and therefore of all objects returned by
  \code{new()}, is \code{"S4"} except for classes that extend
  one of the basic types, where the prototype has that basic type.  User
  functions that depend on \code{\link{typeof}(object)} should be
  careful to handle \code{"S4"} as a possible type.

  The interpretation of the \code{\dots} arguments can be specialized to
  particular classes, if an appropriate method has been defined for the
  generic function \code{"initialize"}.  The \code{new} function calls
  \code{initialize} with the object generated from the prototype as the
  \code{.Object} argument to \code{initialize}.

  By default, unnamed arguments in the \code{\dots} are interpreted as
  objects from a superclass, and named arguments are interpreted as
  objects to be assigned into the correspondingly named slots.  Thus,
  explicit slots override inherited information for the same slot,
  regardless of the order in which the arguments appear.

  The \code{initialize} methods do not have to have \code{\dots} as
  their second argument (see the examples), and generally it is better
  design \emph{not} to have \code{\dots} as a formal argument, if only a
  fixed set of arguments make sense.

  For examples of \code{initialize} methods, see
  \code{\link{initialize-methods}} for existing methods for
  classes \code{"traceable"} and \code{"environment"}, among others.

  Note that the basic vector classes, \code{"numeric"}, etc. are
  implicitly defined, so one can use \code{new} for these classes.
}
\references{
    The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\seealso{ \link{Classes} }

\examples{
## using the definition of class "track" from \link{Classes}

\dontshow{
setClass("track",
         representation(x="numeric", y="numeric"))
setClass("trackCurve",
         representation("track", smooth = "numeric"))

ydata <- stats::rnorm(10); ysmooth <- 1:10
}

## a new object with two slots specified
t1 <- new("track", x = seq_along(ydata), y = ydata)

# a new object including an object from a superclass, plus a slot
t2 <- new("trackCurve", t1, smooth = ysmooth)

### define a method for initialize, to ensure that new objects have
### equal-length x and y slots.

setMethod("initialize",
          "track",
          function(.Object, x = numeric(0), y = numeric(0)) {
            if(nargs() > 1) {
              if(length(x) != length(y))
                stop("specified x and y of different lengths")
              .Object@x <- x
              .Object@y <- y
            }
            .Object
          })

### the next example will cause an error (x will be numeric(0)),
### because we didn't build in defaults for x,
### although we could with a more elaborate method for initialize

try(new("track", y = sort(stats::rnorm(10))))

## a better way to implement the previous initialize method.
## Why?  By using callNextMethod to call the default initialize method
## we don't inhibit classes that extend "track" from using the general
## form of the new() function.  In the previous version, they could only
## use x and y as arguments to new, unless they wrote their own
## initialize method.

setMethod("initialize", "track", function(.Object, ...) {
    .Object <- callNextMethod()
    if(length(.Object@x) != length(.Object@y))
     stop("specified x and y of different lengths")
    .Object
  })

}
\keyword{programming}
\keyword{classes}
