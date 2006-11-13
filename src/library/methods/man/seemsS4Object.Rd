\name{seemsS4Object}
\alias{seemsS4Object}
\title{Heuristic test for an object from an S4 class}
\description{
  Returns \code{TRUE} if \code{object} has been generated from
  a formally defined (\dQuote{S4}) class.  DEPRECATED:  use isS4(object) instead.
}
\usage{
seemsS4Object(object)
}
\arguments{
  \item{object}{Any object.}
}
\details{
  The \code{\link{class}} of the object is examined for the \code{"package"}
  attribute included when objects are generated from an S4 class.  The
  test in this function has  been superseded by an internal bit set
  when S4 objects are generated.

  The \code{seemsS4Object} function is deprecated and will be removed.

  The  test can be fooled in at least two ways:
  \enumerate{
    \item It will give \code{TRUE} incorrectly if someone puts a
    \code{"package"} string attribute on the class of an S3 object.
    Presumably unlikely.
    \item It will give \code{FALSE} incorrectly for class definitions and
    certain other objects for packages that have not been \code{INSTALL}ed
    since the \code{seemsS4Object} was added to R. See the Warning below.
  }
}
\value{
  Always \code{TRUE} or \code{FALSE} for any object.
}
\section{Warnings}{
  One motivation for this function is to prevent standard S3 vector
  operations from being applied to S4 objects that are not vectors.
  Note that \code{seemsS4Object()} alone is \emph{not} that test.  One
  also needs to check that the object does not inherit from class
  \code{"vector"}.  See the examples.

  The existence of a class definition for the object's class is also not
  equivalent.  S4 class definitions are recorded for S3 classes registered
  via \code{\link{setOldClass}}, but registering does not change the class
  of such objects, so they are not judged to be S4 objects (and should not
  be).

  Certain other S4 objects used to be generated without the
  \code{"package"} attribute in earlier builds of R, notably class
  definitions.  Packages using S4 objects \emph{must} be reinstalled with
  a version of R recent enough to contain the \code{seemsS4Object}
  function (e.g., \R 2.3.0 or later).
}
\examples{
seemsS4Object(1) # FALSE

seemsS4Object(getClass(class(1)))  #TRUE

## how to test for an S4 object that is not a vector

S4NotVector <- function(object) seemsS4Object(object) && !is(object, "vector")

setClass("classNotNumeric", representation(x="numeric", y="numeric"))

setClass("classWithNumeric", representation(y="numeric"), contains = "numeric")

obj1 <- new("classNotNumeric", x=1, y=2)

obj2 <- new("classWithNumeric", 1, y=2)

seemsS4Object(obj1); seemsS4Object(obj2)  # TRUE, TRUE
S4NotVector(obj1); S4NotVector(obj2)  # TRUE, FALSE

\dontshow{
removeClass("classNotNumeric")
removeClass("classWithNumeric")
}

}
\keyword{programming}
\keyword{classes}
