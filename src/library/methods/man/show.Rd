% File src/library/methods/man/show.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{show}
\alias{show}
\alias{show-methods}
\alias{show,ANY-method}
\alias{show,traceable-method}
\alias{show,ObjectsWithPackage-method}
\alias{show,MethodDefinition-method}
\alias{show,MethodWithNext-method}
\alias{show,genericFunction-method}
\alias{show,classRepresentation-method}
\title{Show an Object}
\description{
  Display the object, by printing, plotting or whatever suits its
  class.  This function exists to be specialized by methods.  The
  default method calls \code{\link{showDefault}}.

  Formal methods for \code{show} will
  usually be invoked for automatic printing (see the details).
}
\usage{
show(object)
}
\arguments{
  \item{object}{Any R object}
}
\value{
  \code{show} returns an invisible \code{NULL}.
}
\details{
  Objects from an S4 class (a class defined by a call to
  \code{\link{setClass}}) will be displayed automatically is if by a
  call to \code{show}.  S4 objects that occur as attributes of S3
objects will also be displayed in this form; conversely, S3 objects
encountered as slots in S4 objects will be printed using the S3
convention, as if by a call to \code{\link{print}}.

Methods defined for \code{show} will only be inherited  by simple
inheritance, since otherwise the method would not receive the
complete, original object, with misleading results.  See the
\code{simpleInheritanceOnly} argument to \code{\link{setGeneric}} and
the discussion in \code{\link{setIs}} for the general concept.

}
\seealso{
  \code{\link{showMethods}} prints all the methods for one or more
  functions.
}
\examples{
## following the example shown in the setMethod documentation ...
setClass("track", slots = c(x="numeric", y="numeric"))
setClass("trackCurve", contains = "track", slots = c(smooth = "numeric"))

t1 <- new("track", x=1:20, y=(1:20)^2)

tc1 <- new("trackCurve", t1)

setMethod("show", "track",
  function(object)print(rbind(x = object@x, y=object@y))
)
## The method will now be used for automatic printing of t1

t1

\dontrun{  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
x    1    2    3    4    5    6    7    8    9    10    11    12
y    1    4    9   16   25   36   49   64   81   100   121   144
  [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
x    13    14    15    16    17    18    19    20
y   169   196   225   256   289   324   361   400
}
## and also for tc1, an object of a class that extends "track"
tc1

\dontrun{  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
x    1    2    3    4    5    6    7    8    9    10    11    12
y    1    4    9   16   25   36   49   64   81   100   121   144
  [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
x    13    14    15    16    17    18    19    20
y   169   196   225   256   289   324   361   400
}
}
\keyword{programming}
