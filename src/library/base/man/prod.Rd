\name{prod}
\title{Product of Vector Elements}
\usage{
prod(\dots, na.rm=TRUE)
}
\alias{prod}
\description{
  \code{prod} returns the product of all the values
  present in its arguments.
}
\details{
  If \code{na.rm} is \code{FALSE} an \code{NA}
  value in any of the arguments will cause
  a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.
}
\seealso{
\code{\link{sum}},
\code{\link{cumprod}},
\code{\link{cumsum}}.
}
\examples{
print(prod(1:7)) == print(gamma(8))
}
\keyword{arith}
