\name{Comparison}
\alias{<}
\alias{<=}
\alias{==}
\alias{!=}
\alias{>=}
\alias{>}
\alias{Comparison}
\title{Relational Operators}
\description{
  Binary operators which allow the comparison of values in vectors.
}
\usage{
x < y
x > y
x <= y
x >= y
x == y
x != y
}
\value{
  A vector of logicals indicating the result of the element by element
  comparison.  The elements of shorter vectors are recycled as
  necessary.

  Objects such as arrays or time-series can be compared this way
  provided they are conformable.
}
\examples{
x <- rnorm(20)
x < 1
x[x > 0]
}
\keyword{logic}
