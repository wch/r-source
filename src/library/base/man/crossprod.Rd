\name{crossprod}
\title{Matrix Crossproduct}
\usage{
crossprod(x, y=x)
}
\alias{crossprod}
\description{
  Given matrices \code{x} and \code{y} as arguments,
  \code{crossprod} returns their matrix cross-product.
  This is formally equivalent to, but faster than, the
  call \code{t(x) \%*\% y}.
}
\seealso{
%%FIXME BUG in Rdconv:\code{\link{\%*\%}} and outer product \code{\link{\%o\%}}.
%%------------- instead:
\code{\link{outer}}, \link{matmult}.}
}
\examples{
crossprod(1:4)# = sum(1 + 2^2 + 3^2 + 4^2)
drop(.Last.value) # scalarized
}
\keyword{algebra}
\keyword{array}
