\name{sign}
\title{Sign Function}
\usage{
sign(x)
}
\alias{sign}
\arguments{
  \item{x}{a numeric vector}
}
\description{
  \code{sign} returns a vector with the signs of the corresponding
  elements of \code{x} (the sign of a real number is 1, 0, or \eqn{-1}
  if the number is positive, zero, or negative, respectively).

  Note that \code{sign} does not operate on complex vectors.
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link[base:groupGeneric]{Math}} group generic.
}
\seealso{
  \code{\link{abs}}
}
\examples{
sign(pi) # == 1
sign(-2:3)# -1 -1 0 1 1 1
}
\keyword{arith}

