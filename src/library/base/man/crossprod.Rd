\name{crossprod}
\title{Matrix Crossproduct}
\usage{
crossprod(x, y=x)
}
\alias{crossprod}
\description{
Given matrices \code{x} and \code{y} as arguments,
\code{t} returns their matrix cross-product.
This is formally equivalent to, but faster than, the
call \code{t(x) \%*\% y}.
}
\keyword{algebra}
\keyword{array}
