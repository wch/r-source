\name{margin.table}
\title{Compute table margin}
\usage{
margin.table(x, margin=NULL)
}
\alias{margin.table}
\arguments{
  \item{x}{an array}
  \item{margin}{index number (1 for rows, etc.) }
}
\description{
  Compute the sum of table entries for a given index.
}
\details{
  This is really just \code{apply(x,margin,sum)} packaged up for
  newbies, except that if \code{margin} has length zero you get
  \code{sum(x)}.
}
\value{
  The relevant marginal table.
}
\author{Peter Dalgaard}
\examples{
m<-matrix(1:4,2)
margin.table(m,1)
margin.table(m,2)
}
\keyword{array}
