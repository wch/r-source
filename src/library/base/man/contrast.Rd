\name{Contrast}
\title{Contrast Matrices}
\usage{
contr.helmert(n, contrasts=TRUE)
contr.poly(n, contrasts=TRUE)
contr.sum(n, contrasts=TRUE)
contr.treatment(n, base=1, contrasts=TRUE)
}
\alias{contr.helmert}
\alias{contr.poly}
\alias{contr.sum}
\alias{contr.treatment}
\value{
A matrix with \code{n} rows and \code{k} columns,
with \code{k=n-1} if \code{contrasts} is \code{TRUE}
and \code{k=n} if \code{contrasts} is \code{FALSE}.

The base argument to \code{contr.treatment()} allows you to specify
which group is considered the baseline group.

These functions are used for creating contrast matrices
for use in fitting analysis of variance and regression models.
The columns of the resulting matrices contain contrasts
which can be used for coding a factor with \code{n} levels.
The returned value contains the computed contrasts.
If the argument \code{contrasts} is \code{FALSE} then an
additional column of ones is prepended to the matrix.

Note that as from \R version 0.62.2, \code{contr.poly} returns contrasts
based on orthogonal (rather than raw) polynomials.
}
\seealso{
    \code{\link{contrasts}}, \code{\link{C}}, and
    \code{\link{aov}}, \code{\link{glm}}, \code{\link{lm}}.
}
\examples{
(cH <- contr.helmert(4))
apply(cH, 2,sum)# column sums are 0!
crossprod(cH)# diagonal -- columns are orthogonal

(cT <- contr.treatment(5))
all(crossprod(cT) == diag(4))# TRUE: even orthonormal

(cP <- contr.poly(3))# Linear and Quadratic
zapsmall(crossprod(cP), dig=15) # orthonormal up to fuzz
}
\keyword{design}
\keyword{regression}
\keyword{array}
