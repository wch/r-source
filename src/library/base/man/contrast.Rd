\name{Contrast}
\title{Contrast Matrices}
\usage{
contr.helmert(n, contrasts=TRUE)
contr.poly(n, contrasts=TRUE)
contr.sum(n, contrasts=TRUE)
contr.treatment(n, contrasts=TRUE)
}
\alias{contr.helmert}
\alias{contr.poly}
\alias{contr.sum}
\alias{contr.treatment}
\value{
A matrix with \code{n} rows and \code{k} columns,
with \code{k=n-1} if \code{contrasts} is \code{TRUE}
and \code{k=n} if \code{contrasts} is \code{FALSE}.

These functions are used for creating contrast matrices
for use in fitting analysis of variance and regression models.
The columns of the resulting matrices contain contrasts
which can be used for coding a factor with \code{n} levels.
The returned value contains the computed contrasts.
If the argument \code{contrasts} is \code{FALSE} then an
additional column of ones is prepended to the matrix.
}
\seealso{
\code{\link{contrasts}},
\code{\link{glm}},
\code{\link{lm}}.
}
\keyword{design}
\keyword{regression}
\keyword{array}
