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
  \item{\dots}{numeric or complex or logical vectors.}
  \item{na.rm}{logical.  Should missing values be removed?}
}
\value{
  The product.
  \strong{NB:} the product of an empty set is one, by definition.
}
\details{
  If \code{na.rm} is \code{FALSE} an \code{NA}
  value in any of the arguments will cause
  a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.

  This is a generic function: methods can be defined for it
  directly or via the \code{\link{Summary}} group generic.
  For this to work properly, the arguments \code{\dots} should be unnamed.

  For historical reasons, \code{NULL} is accepted and treated as if it
  were \code{numeric(0)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{sum}},
  \code{\link{cumprod}}, \code{\link{cumsum}}.
}
\examples{
print(prod(1:7)) == print(gamma(8))
}
\keyword{arith}
