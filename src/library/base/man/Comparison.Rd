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
\details{
  Comparison of strings in character vectors is lexicographic within the
  strings using the collating sequence of the locale in use: see
  \code{\link{locales}}.  The collating sequence of locales such as
  \code{en_US} is normally different from \code{C} (which should use
  ASCII) and can be surprising.
}
\value{
  A vector of logicals indicating the result of the element by element
  comparison.  The elements of shorter vectors are recycled as
  necessary.

  Objects such as arrays or time-series can be compared this way
  provided they are conformable.
}
\note{
  Don't use \code{==} and \code{!=} for tests, such as in \code{if}
  expressions, where you must get a single \code{TRUE} or
  \code{FALSE}.  Unless you are absolutely sure that nothing unusual
  can happen, you should use the \code{\link{identical}} function
  instead.

  For numerical values, remember \code{==} and \code{!=} do not allow
  for the finite representation of fractions, nor for rounding error.
  Using \code{\link{all.equal}} with \code{identical} is almost
  always preferable.  See the examples.
}
\seealso{
  \code{\link{Syntax}} for operator precedence.
}
\examples{
x <- rnorm(20)
x < 1
x[x > 0]

x1 <- 0.5 - 0.3
x2 <- 0.3 - 0.1
x1 == x2                           # FALSE on most machines
identical(all.equal(x1, x2), TRUE) # TRUE everywhere
}
\keyword{logic}
