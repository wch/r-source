\name{rev}
\alias{rev}
\alias{rev.default}
\title{Reverse Elements}
\usage{
rev(x)
}
\arguments{
  \item{x}{a vector or another object for which reversion is defined.}
}
\description{
  \code{rev} provides a reversed version of its argument.  It is generic
  function with a default method for vectors and one for
  \code{\link{dendrogram}}s.

  Note that this is no longer needed (nor efficient) for obtaining
  vectors sorted into descending order, since that is now rather more
  directly achievable by \code{\link{sort}(x, decreasing = TRUE)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{seq}}, \code{\link{sort}}.}
\examples{
x <- c(1:5,5:3)
## sort into descending order; first more efficiently:
stopifnot(sort(x, decreasing = TRUE) == rev(sort(x)))
stopifnot(rev(1:7) == 7:1)#- don't need 'rev' here
}
\keyword{manip}
