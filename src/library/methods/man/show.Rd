\name{show}
\alias{show}
\title{Show an Object}
\description{
  Display the object, by printing, plotting or whatever suits its class.

  The function \code{show} exists to be specialized by methods; the
  default method calls \code{showDefault}.

  With library \code{methods} attached, methods for \code{show} will
  usually be invoked for automatic printing (see the details).

  The function \code{showDefault} allows redirection of output and
  optional use of old-style print methods, but normally will not be
  called directly.
}
\usage{
show(object)

showDefault(object, printTo = stdout(), oldMethods = TRUE)
}
\arguments{
  \item{object}{Any R object}
  
  \item{printTo}{Either a file or \link{connection}, or else
    \code{FALSE}.  In the latter case, the lines of text that would
    have been printed are returned as the value of the call (in a
    character vector with one element per line of output).
  }
  \item{oldMethods}{Should old-style print methods be used for this
    object?  \code{TRUE} by default if called directly, but \code{FALSE}
    when called from the methods package for automatic printing (to
    avoid potential recursion; see the
    details below).
  }
}
\value{
  \code{show} returns an invisible \code{NULL}.

  For \code{showDefault}, if \code{printTo} is \code{FALSE}, the value
  is a character vector containing the lines that would otherwise have
  been printed.
}
\details{
  The \code{methods} package overrides the base definition of
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
