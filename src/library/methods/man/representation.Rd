\name{representation}
\alias{representation}
\title{ Describe A Class Representation }
\description{
  Constructs a description of a class (without verifying the validity of
  the description).  A convenience function in calls to
  \code{\link{setClass}}.
}
\usage{
representation(...)
}
\arguments{
  \item{...}{
    The call to representation takes arguments that are single character
    strings.  Unnamed arguments are classes that a newly defined class
    extends; named arguments name the explicit slots in the new class,
    and specify what class each slot should have.}
}
\details{
  The \code{representation} function applies tests for the validity of
  the arguments.  Each must specify the name of a class.

  The classes named don't have to exist when \code{representation} is
  called, but if they do, then the function will check for any duplicate
  slot names introduced by each of the inherited classes.
}
\value{
  The value is just the list of arguments, after these have been checked
  for validity.
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html}
  is the primary documentation.

  The functions in this package emulate the facility for classes
  and methods described in \emph{Programming with Data} (John
  M. Chambers, Springer, 1998).  See this book for further details and
  examples.
}
\author{
  John Chambers
}
\seealso{ \code{\link{setClass}} }

\examples{
## representation for a new class with a directly define slot "smooth"
## which should be a "numeric" object, and extending class "track"
representation("track", smooth ="numeric")
\testonly{
setClass("class1", representation(a="numeric", b = "character"))
setClass("class2", representation(a2 = "numeric", b = "numeric"))
try(setClass("class3", representation("class1", "class2")))

}
}
\keyword{programming}
\keyword{classes}

