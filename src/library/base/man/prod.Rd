\name{prod}
\title{Product of Vector Elements}
\usage{
prod(\dots, na.rm = FALSE)
}
\alias{prod}
\description{
  \code{prod} returns the product of all the values
  present in its arguments.
}
\arguments{
  \item{\dots}{numeric vectors.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  If \code{na.rm} is \code{FALSE} an \code{NA}
  value in any of the arguments will cause
  a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.

  This is a generic function: methods can be defined for it
  directly or via the \code{\link{Summary}} group generic.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
