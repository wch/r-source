\name{toString}
\alias{toString}
\alias{toString.default}

\title{toString Converts its Argument to a Character String}
\description{
  This is a helper function for \code{\link{format}}. It converts its argument
  to a string. If the argument is a vector then its elements are
  concatenated with a \code{,} as a separtor. 
  Most methods should honor the width argument. 
  The minimum value for \code{width} is six. 
}
\usage{
toString(x, \dots)

\method{toString}{default}(x, width, \dots)
}
\arguments{
  \item{x}{The object to be converted.}
  \item{width}{The returned value is at most the first \code{width} characters.}
  \item{\dots}{Optional arguments for methods.}
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
