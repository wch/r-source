% File src/library/methods/man/new.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{new}
\alias{new}
\alias{initialize}
\title{ Generate an Object from a Class }
\description{
  A call to  \code{new} returns a newly allocated object from the
  class identified by the first argument.  This call in turn calls the
  method for the generic function \code{initialize} corresponding to
  the specified class, passing the \code{\dots} arguments to this
method.
In the default method for \code{initialize()}, named arguments provide
values for the corresponding slots and unnamed arguments must be
objects from superclasses of this class.

A call to a generating function for a class (see
\code{\link{setClass}}) will pass its \dots arguments to a corresponding call to \code{new()}.
}
\usage{
new(Class, ...)

initialize(.Object, ...)
}
\arguments{
  \item{Class}{either the name of a class, a \code{\link{character}}
    string, (the usual case) or the object describing the class (e.g.,
    the value returned by \code{getClass}). Note that the character
    string passed from a generating function includes the package name
  as an attribute, avoiding ambiguity if two packages have identically
named classes.}
  \item{\dots}{arguments to specify properties of the new object, to
      be passed to \code{initialize()}.}
  \item{.Object}{ An object:  see the \dQuote{Initialize Methods} section.}
}
\section{Initialize Methods}{
The generic function \code{initialize} is not called directly.
A call to \code{new} begins by copying the prototype object from
  the class definition, and then calls \code{intialize()} with this
  object as the first argument, followed by the \dots{} arguments. 

  The interpretation of the \code{\dots} arguments in a call to a
  generator function or to \code{new()} can be specialized to
  particular classes, by defining an appropriate method for \code{"initialize"}. 

  In the default method, unnamed arguments in the \code{\dots} are interpreted as
  objects from a superclass, and named arguments are interpreted as
  objects to be assigned into the correspondingly named slots.
  Explicitly specified slots override inherited information for the same slot,
  regardless of the order in which the arguments appear.

  The \code{initialize} methods do not have to have \code{\dots} as
  their second argument (see the examples).  Initialize methods are
  often written when the natural parameters describing the new object
  are not the names of the slots.  If you do define such a method,
you should include  \code{\dots} as a formal argument, and your method should pass such
  arguments along via \code{\link{callNextMethod}}. 
 This helps the definition of future subclasses of your class.  If these
  have additional slots and your method
  does \emph{not} have this argument, it will be difficult for these
  slots to be included in an initializing call.

  See
  \code{\link{initialize-methods}} for a discussion of some classes with existing
  methods. 

  Methods for \code{initialize} can be inherited only by simple
  inheritance, since it is a requirement that the method return an
  object from the target class.  See the
  \code{simpleInheritanceOnly} argument to \code{\link{setGeneric}} and
  the discussion in \code{\link{setIs}} for the general concept.

  Note that the basic vector classes, \code{"numeric"}, etc. are
  implicitly defined, so one can use \code{new} for these classes.
  The \dots arguments are interpreted as objects of this type and are
  concatenated into the resulting vector.
}
\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}
\seealso{ \link{Classes_Details} for details of class definitions, and
  \code{\link{setOldClass}} for the relation to S3 classes. }

\examples{
## using the definition of class "track" from \link{setClass}

\dontshow{
setClass("track", slots = c(x="numeric", y="numeric"))
setClass("trackCurve", contains = "track",
          slots = c(smooth = "numeric"))

ydata <- stats::rnorm(10); ysmooth <- 1:10
}

## a new object with two slots specified
t1 <- new("track", x = seq_along(ydata), y = ydata)

# a new object including an object from a superclass, plus a slot
t2 <- new("trackCurve", t1, smooth = ysmooth)

### define a method for initialize, to ensure that new objects have
### equal-length x and y slots.  In this version, the slots must still be
### supplied by name.

setMethod("initialize", "track", 
    function(.Object, ...) {
      .Object <- callNextMethod()
      if(length(.Object@x) != length(.Object@y))
      stop("specified x and y of different lengths")
      .Object
    })

### An alternative version that allows x and y to be supplied
### unnamed.  A still more friendly version would make the default x
### a vector of the same length as y, and vice versa.

setMethod("initialize", "track",
          function(.Object, x = numeric(0), y = numeric(0), ...) {
              .Object <- callNextMethod(.Object, ...)
              if(length(x) != length(y))
                  stop("specified x and y of different lengths")
              .Object@x <- x
              .Object@y <- y
              .Object
          })

\dontshow{
removeMethod("initialize", "track")
}
}
\keyword{programming}
\keyword{classes}
