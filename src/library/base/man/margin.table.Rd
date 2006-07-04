\name{margin.table}
\alias{margin.table}
\title{Compute table margin}
\description{
  For a contingency table in array form, compute the sum of table
  entries for a given index.
}
\usage{
margin.table(x, margin=NULL)
}
\arguments{
  \item{x}{an array}
  \item{margin}{index number (1 for rows, etc.) }
}
\details{
  This is really just \code{apply(x, margin, sum)} packaged up for
  newbies, except that if \code{margin} has length zero you get
  \code{sum(x)}.
}
\seealso{
  \code{\link{prop.table}} and \code{\link[stats]{addmargins}}.
}
\value{
  The relevant marginal table.  The class of \code{x} is copied to the
  output table, except in the summation case.
}
\author{Peter Dalgaard}
\examples{
m <- matrix(1:4,2)
margin.table(m,1)
margin.table(m,2)
}
\keyword{array}
