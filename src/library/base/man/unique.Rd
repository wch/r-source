\name{unique}
\alias{unique}
\alias{unique.default}
\alias{unique.data.frame}
\title{Extract Unique Elements}
\description{
  \code{unique} returns a vector or data frame like \code{x} but with duplicate
  elements removed.  If an element is equal to one with a smaller index,
  it is removed.
}
\usage{
unique(x, incomparables = FALSE)
}
\arguments{
  \item{x}{an atomic vector or a data frame.}
  \item{incomparables}{a vector of values that cannot be compared.
    Currently, \code{FALSE} is the only possible value, meaning that all
    values can be compared.}
}
\seealso{
  \code{\link{duplicated}} which gives the indices of duplicated
  elements.
}
\examples{
unique(c(3:5, 11:8, 8 + 0:5))
length(unique(sample(100, 100, replace=TRUE)))
## approximately 100(1 - 1/e) = 63.21
my.unique <- function(x) x[!duplicated(x)]
for(i in 1:4)
 { x <- rpois(100, pi); stopifnot(unique(x) == my.unique(x)) }

data(iris)
unique(iris)
stopifnot(dim(unique(iris)) == c(149, 5))
}
\keyword{manip}
\keyword{logic}
