\name{outer}
\alias{outer}
\alias{\%o\%}
\title{Outer Product of Arrays}
\description{
  The outer product of the arrays \code{X} and \code{Y} is the array
  \code{A} with dimension \code{c(dim(X), dim(Y))} where element
  \code{A[c(arrayindex.x, arrayindex.y)]
    = FUN(X[arrayindex.x], Y[arrayindex.y], \dots)}.
}
\usage{
outer(X, Y, FUN="*", \dots)
X \%o\% Y
}

\arguments{
  \item{X, Y}{First and second arguments for function \code{FUN}.
    Typically a vector or array.}
  \item{FUN}{a function to use on the outer products, found \emph{via}
    \code{\link{match.fun}} (except for the special case \code{"*"}).}
  \item{\dots}{optional arguments to be passed to \code{FUN}.}
}
\details{
  \code{FUN} must be a function (or the name of it) which expects at
  least two arguments and which operates elementwise.

  \code{X} and \code{Y} must be suitable arguments for \code{FUN}.  Each
  will be extended by \code{\link{rep}} to length the products of the
  lengths of \code{X} and \code{Y} before \code{FUN} is called.
    
  Where they exist, the [dim]names of \code{X} and \code{Y} will be
  copied to the answer, and a dimension assigned which is the
  concatenation of the dimensions of \code{X} and \code{Y} (or lengths
  if dimensions do not exist).

  \code{FUN = "*"} is handled internally as a special case, \emph{via}
  \code{as.vector(X) \%*\% t(as.vector(Y))}, and is intended only for
  numeric vectors and arrays.
  
  \code{\%o\%} is binary operator providing a wrapper for
  \code{outer(x, y, "*")}.
}
\author{Jonathan Rougier}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{\%*\%}} for usual (\emph{inner}) matrix vector
  multiplication;
  \code{\link{kronecker}} which is based on \code{outer}.
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
