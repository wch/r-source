\name{crossprod}
\alias{crossprod}
\title{Matrix Crossproduct}
\description{
  Given matrices \code{x} and \code{y} as arguments,
  \code{crossprod} returns their matrix cross-product.
  This is formally equivalent to, but faster than, the
  call \code{t(x) \%*\% y}.
}
\usage{
crossprod(x, y = NULL)
}
\arguments{
  \item{x, y}{matrices: \code{y = NULL} is taken to be the
    same matrix as \code{x}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
\code{\link{\%*\%}} and outer product \code{\link{\%o\%}}.
}
\examples{
(z <- crossprod(1:4))    # = sum(1 + 2^2 + 3^2 + 4^2)
drop(z)                  # scalar
}
\keyword{algebra}
\keyword{array}
