% File src/library/base/man/lower.tri.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{lower.tri}
\alias{lower.tri}
\alias{upper.tri}
\title{Lower and Upper Triangular Part of a Matrix}
\description{
  Returns a matrix of logicals the same size of a given matrix with
  entries \code{TRUE} in the lower or upper triangle.
}
\usage{
lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)
}
\arguments{
  \item{x}{a matrix or other \R object with \code{length(dim(x)) == 2}.
    For back compatibility reasons, when the above is not fulfilled,
    \code{\link{as.matrix}(x)} is called first.}
  \item{diag}{logical.  Should the diagonal be included?}
}
\details{
  Note that \code{!lower.tri(x)} gives the same as \code{upper.tri(x, diag=TRUE)}, and
  analogously, \code{!upper.tri(x)} is equivalent to \code{lower.tri(x, diag=TRUE)},
  and the \code{diag=TRUE} versions are very slightly more efficient.
}
\seealso{
  \code{\link{diag}}, \code{\link{matrix}}; further \code{\link{.row}}
  and \code{\link{.col}} on which \code{lower.tri()} and
  \code{upper.tri()} are built.
}
\examples{
(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2

## "prove" equivalence   lower.tri  <---> !upper.tri
set.seed(12)
for(i in 1:99) {
    x <- matrix(as.integer(64*rnorm(6*4)), 6, 4)
    stopifnot(identical( !lower.tri(x), upper.tri(x, diag=TRUE)),
              identical( !upper.tri(x), lower.tri(x, diag=TRUE)))
 }
}
\keyword{array}
