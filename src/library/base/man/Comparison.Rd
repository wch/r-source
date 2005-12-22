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
  Binary operators which allow the comparison of values in atomic vectors.
}
\usage{
x < y
x > y
x <= y
x >= y
x == y
x != y
}
\arguments{
  \item{x, y}{atomic vectors, or other objects for which methods have
    been written.}
}
\details{
  The binary comparison operators are generic functions: methods can be
  written for them individually or via the \code{\link{Ops}}) group generic
  function.

  Comparison of strings in character vectors is lexicographic within the
  strings using the collating sequence of the locale in use: see
  \code{\link{locales}}.  The collating sequence of locales such as
  \samp{en\_US} is normally different from \samp{C} (which should use
  ASCII) and can be surprising.

  At least one of \code{x} and \code{y} must be an atomic vector, but if
  the other is a list \R attempts to coerce it to the type of the atomic
  vector: this will succeed if the list is made up of elements of length
  one that canbe coerced to the correct type.

  If the two arguments are atomic vectors of different types, one is
  coerced to the type of the other, the order of precedence being
  character, complex, numeric, integer and logical.

  When comparisons are made between character strings, parts of the
  strings after embedded \code{nul} characters are ignored.  (This is
  necessary as the position of \code{nul} in the collation sequence is
  undefined, and we want one of \code{<}, \code{==} and \code{>} to be
  true for any comparison.)
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
