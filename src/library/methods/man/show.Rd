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
  The \pkg{methods} package overrides the base definition of
  \code{print.default} to arrange for automatic printing to honor
  methods for the function \code{show}.  This does not quite manage to
  override old-style printing methods, since the automatic printing in
  the evaluator will look first for the old-style method.

  If you have a class \code{myClass} and want to define a method for
  \code{show}, all will be well unless there is already a function named
  \code{print.myClass}.  In that case, to get your method dispatched for
  automatic printing, it will have to be a method for \code{print}.  A
  slight cheat is to override the function \code{print.myClass} yourself,
  and then call that function also in the method for \code{show}
  with signature \code{"myClass"}.
}
\seealso{
  \code{\link{showMethods}} prints all the methods for one or more
  functions;
  \code{\link{showMlist}} prints individual methods lists; 
  \code{\link{showClass}} prints class definitions.
  Neither of the latter two normally needs to be called directly.
}
\examples{
## following the example shown in the setMethod documentation ...
setClass("track",
         representation(x="numeric", y="numeric"))
setClass("trackCurve", 
         representation("track", smooth = "numeric"))

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
