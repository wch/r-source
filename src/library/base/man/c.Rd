\name{c}
\title{Combine Values into a Vector or List}
\usage{
c(\dots, recursive=FALSE)
}
\alias{c}
\description{
  This is a generic function which combines its arguments.

  The default method combines its arguments to form a vector.
  All arguments are coerced to a common type which is the type
  of the returned value.
}
\arguments{
  \item{\dots}{objects to be concatenated.}
  \item{recursive}{logical. If \code{recursive = TRUE}, the function
    recursively descends through lists combining all their elements
    into a vector.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{unlist}} and \code{\link{as.vector}} to produce
  attribute-free vectors.
}
\examples{
c(1,7:9)
c(1:5, 10.5, "next")

## append to a list:
ll <- list(A = 1, c="C")
## do *not* use
c(ll, d = 1:3) # which is == c(ll, as.list(c(d=1:3))
## but rather
c(ll, d = list(1:3))# c() combining two lists

c(list(A=c(B=1)), recursive=TRUE)

c(options(), recursive=TRUE)
c(list(A=c(B=1,C=2), B=c(E=7)), recursive=TRUE)
}
\keyword{manip}
