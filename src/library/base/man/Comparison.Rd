\name{Comparison}
\title{Relational Operators}
\usage{
x < y
x > y
x <= y
x >= y
x == y
x != y
}
\alias{<}
\alias{<=}
\alias{==}
\alias{!=}
\alias{>=}
\alias{>}
\alias{Comparison}
\value{
These binary operators allow the comparison of values in vectors.
They return a vector of logicals indicating the result of the
element by element comparison.  The elements of shorter vectors
are recycled as necessary.

Objects such as arrays or time-series can be compared this
way provided they are conformable.
}
\examples{
x <- rnorm(20)
x < 1
x[x > 0]
}
\keyword{logic}
