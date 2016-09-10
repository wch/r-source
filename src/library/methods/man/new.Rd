% File src/library/methods/man/new.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

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
  \item{Class}{either the name of a class, a \code{\link{character}}
    string, (the usual case) or the object describing the class (e.g.,
    the value returned by \code{getClass}).}
  \item{\dots}{data to include in the new object.  Named arguments
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

  Note that the \emph{name} of the first argument, \code{"Class"}
  entails that \code{"Class"} is an undesirable slot name in any formal
  class: \code{new("myClass", Class = <value>)} will not work.

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
  their second argument (see the examples).  Initialize methods are
  often written when the natural parameters describing the new object
  are not the names of the slots.  If you do define such a method,
  note the implications for future subclasses of your class.  If these
  have additional slots, and your \code{initialize} method has
  \code{\dots} as a formal argument, then your method should pass such
  arguments along via \code{\link{callNextMethod}}.  If your method
  does not have this argument, then either a subclass must have its
  own method or else the added slots must be specified by users in
  some way other than as arguments to \code{new}.

  For examples of \code{initialize} methods, see
  \code{\link{initialize-methods}} for existing methods for
  classes \code{"traceable"} and \code{"environment"}, among
  others. See the comments there on subclasses of
  \code{"environment"}; any \code{initialize} methods for these should
  be sure to allocate a new environment.

  Methods for \code{initialize} can be inherited only by simple
  inheritance, since it is a requirement that the method return an
  object from the target class.  See the
  \code{simpleInheritanceOnly} argument to \code{\link{setGeneric}} and
  the discussion in \code{\link{setIs}} for the general concept.

  Note that the basic vector classes, \code{"numeric"}, etc. are
  implicitly defined, so one can use \code{new} for these classes.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{ \link{Classes} for an overview of defining class, and
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
