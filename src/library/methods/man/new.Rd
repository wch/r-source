\name{new}
\alias{new}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Generate an Object from a Class }
\description{
  Given the the name or the definition of a class, plus optionally
  data to be included in the object, \code{new} returns an object from that class.
}
\usage{
new(Class, ..., .Force=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{Class}{ Either the name of a class (the usual case) or the
    object describing the class (e.g., the value returned by \code{getClass}). }
  \item{\dots}{ Data to include in the new object.  Named arguments
    correspond to slots in the class definition. Unnamed arguments must
    be objects from classes that this class extends.}
  \item{.Force}{ Normally, an attempt to apply \code{new} to a virtual
    or an undefined class generates an error.  If \code{.Force} is
    \code{TRUE}, an ``empty'' object is returned instead.  Users are
    very unlikely to need this argument. }
}
\details{
  The function begins by copying the prototype object from the class
  definition.  Then information is inserted according to the \dots
  arguments, if any, first from the superclasses (the unnamed arguments)
  then from the named slots.  Thus, explicit slots override inherited
  information for the same slot, regardless of the order in which the
  arguments appear.

  Note that the basic vector classes, \code{"numeric"}, etc. are implicitly defined,
  so one can use \code{new} for these classes.

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

\seealso{ \link{Classes} }

\examples{
## using the definition of class "track" from \link{Classes}

\testonly{
setClass("track",
            representation(x="numeric", y="numeric"))
setClass("trackCurve",
            representation("track", smooth = "numeric"))

ydata <- rnorm(10); ysmooth <- 1:10}

## a new object with two slots specified
t1 <- new("track", x = seq(along=ydata), y = ydata)

# a new object including an object from a superclass, plus a slot
t2 <- new("trackCurve", t1, smooth = ysmooth)
}
\keyword{programming}
\keyword{classes}
