\name{prop.table}
\title{Express Table Entries as Fraction of Marginal Table}
\concept{proportions}
\usage{
prop.table(x, margin=NULL)
}
\alias{prop.table}
\arguments{
 \item{x}{ table }
 \item{margin}{ index, or vector of indices to generate margin for}
}
\description{
  This is really \code{sweep(x, margin, margin.table(x, margin), "/")}
  for newbies, except that if \code{margin} has length zero, then one
  gets \code{x/sum(x)}.
}
\value{
  Table like \code{x} expressed relative to \code{margin}
}
\author{ Peter Dalgaard }

\seealso{\code{\link{margin.table}}}

\examples{
m <- matrix(1:4,2)
m
prop.table(m,1)
}
\keyword{ array }
