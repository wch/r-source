\name{unique}
\alias{unique}
\title{Extract Unique Elements}
\description{
  \code{unique} returns a vector like \code{x} but with duplicate
  elements removed.  If an element is equal to one with a smaller index,
  it is removed.
}
\usage{
unique(x, incomparables = FALSE)
}
\arguments{
  \item{x}{an atomic vector.}
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
}
\keyword{manip}
\keyword{logic}
