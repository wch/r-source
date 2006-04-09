\name{cumsum}
\alias{cumsum}
\alias{cumprod}
\alias{cummin}
\alias{cummax}
\title{Cumulative Sums, Products, and Extremes}
\description{
  Returns a vector whose elements are the cumulative sums, products,
  minima or maxima of the elements of the argument.
}
\usage{
cumsum(x)
cumprod(x)
cummax(x)
cummin(x)
}
\arguments{
  \item{x}{a numeric object.}
}
\details{
  An \code{NA} value in \code{x} causes the corresponding and following
  elements of the return value to be \code{NA}.

  These are generic functions: methods can be defined for them
  individually or via the \code{\link{Math}} group generic.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (\code{cumsum} only.)
}
\examples{
cumsum(1:10)
cumprod(1:10)
cummin(c(3:1, 2:0, 4:2))
cummax(c(3:1, 2:0, 4:2))
}
\keyword{arith}
