\name{outer}
\title{Outer Product of Arrays}
\usage{
outer(X, Y, FUN="*", \dots)
x \%o\% y
}
\alias{outer}
\alias{\%o\%}

\arguments{
 \item{X}{A vector or array.}
 \item{Y}{A vector or array.}
 \item{FUN}{a function to use on the outer products, it may be
          a quoted string.}
 \item{\dots}{optional arguments to be passed to \code{FUN}.}
}
\description{
    The outer product of the arrays \code{X} and \code{Y} is the
    array \code{A} with dimension \code{c(dim(X), dim(Y))} where
    element \code{A[i, j, .., k, l, ..] = FUN(X[i, j, ..], Y[k, l,..], ..)}.
}
\details{
    \code{FUN} must be a function (or the name of it) which expects at
    least two arguments and which operates elementwise on arrays.
    
    Where they exist, the [dim]names of \code{X} and \code{Y} will be
    preserved.
    
    \code{\%o\%} is an \code{.Alias} for \code{outer} (where \code{FUN}
    cannot be changed from \code{"*"}).
}
\author{Jonathan Rougier}

\seealso{\code{\link{matmult}} for usual (\emph{inner}) matrix vector
    multiplication; \code{\link{kronecker}} which is based on \code{outer}.
}
\examples{
x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x \%o\% x
y <- 2:8; names(y) <- paste(y,":",sep="")
outer(y, x, "^")

outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table:
x \%o\% x \%o\% y[1:3]
}
\keyword{array}
