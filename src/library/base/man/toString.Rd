\name{toString}
\alias{toString}
\alias{toString.default}

\title{toString Converts its Argument to a Character String}
\description{
  This is a helper function for \code{\link{format}}. It converts its argument
  to a string. If the argument is a vector then its elements are
  concatenated with a \code{,} as a separator. 
  Most methods should honor the width argument. 
}
\usage{
toString(x, \dots)

\method{toString}{default}(x, width, \dots)
}
\arguments{
  \item{x}{The object to be converted.}
  \item{width}{The returned value has at most \code{width} characters.
    The minimum value accepted is 6 and smaller values are taken as 6.}
  \item{\dots}{Optional arguments for methods.}
}
\details{
  The default method returns the first \code{width - 4} characters of
  the result with \code{....} appended, if the full result would use
  more than \code{width} characters.
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
