\name{outer}
\title{Outer Product of Vectors}
\usage{
outer(x, y, FUN = "*", \dots)
x \%o\% y
}
\alias{outer}
\alias{\%o\%}
\arguments{
\item{x}{a vector.}
\item{y}{a vector.}
\item{FUN}{a function to use on the outer products, it may be a quoted string.}
\item{\dots}{optional arguments to be passed to \code{fun}.}
}
\description{
The outer product of the vectors \code{x} and \code{y} is the matrix
\code{A} with elements
\code{A[i, j] = FUN(x[i], y[j], \dots)}.
\code{FUN} must be a function which expects at least two arguments
and which operates elementwise on arrays.

This needs to be extended to taking arrays of arbitrary dimension for
\code{x} and \code{y} and returns an array of dimension
\code{c(dim(x), dim(y))}.

\code{\%o\%} is an \code{\link{.Alias}} for \code{outer} (where
\code{FUN} cannot be changed from \code{"*"}.
}
\examples{
outer(1:5, 1:5, "+")
all(outer(1:5, 1:5) == 1:5 \%o\% 1:5)
}
\keyword{array}
