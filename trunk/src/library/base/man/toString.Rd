\name{toString}
\alias{toString}
\alias{toString.default}

\title{Convert an R Object to a Character String}
\description{
  This is a helper function for \code{\link{format}} to produce a single
  character string describing an \R object.
}
\usage{
toString(x, \dots)

\method{toString}{default}(x, width = NULL, \dots)
}
\arguments{
  \item{x}{The object to be converted.}
  \item{width}{Suggestion for the maximum field width.  Values of
    \code{NULL} or \code{0} indicate no maximum.
    The minimum value accepted is 6 and smaller values are taken as 6.}
  \item{\dots}{Optional arguments passed to or from methods.}
}
\details{
  This is a generic function for which methods can be written: only the
  default method is described here.  Most methods should honor the
  \code{width} argument to specify the maximum display width (as measured
  by \code{\link{nchar}(type = "width"}) of the result.

  The default method first converts \code{x} to character and then
  concatenates the elements separated by \code{", "}.
  If \code{width} is supplied and is not \code{NULL}, the default method
  returns the first \code{width - 4} characters of the result with
  \code{....} appended, if the full result would use more than
  \code{width} characters.
}
\value{
  A character vector of length 1 is returned.
}
\author{ Robert Gentleman }

\seealso{ \code{\link{format}} }

\examples{
x <- c("a", "b", "aaaaaaaaaaa")
toString(x)
toString(x, width=8)
}
\keyword{utilities}
