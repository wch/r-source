\name{duplicated}
\title{Determine Duplicate Elements}
\usage{
duplicated(x, incomparables = FALSE)
}
\alias{duplicated}
\arguments{
  \item{x}{an atomic vector}
  \item{incomparables}For S compatibility. Other values are not yet
  implemented in \R.}
}
\description{
  Determines which elements of a vector are duplicates
  of elements with smaller subscripts, and returns a
  logical vector indicating which elements are duplicates.
}
\seealso{\code{\link{unique}}.}
\examples{
x <- c(9:20, 1:5, 3:7, 0:8)
## extract unique elements
(xu <- x[!duplicated(x)])
stopifnot(xu == unique(x), # but unique(x) is more efficient
          0:20 == sort(x[!duplicated(x)]))
}
\keyword{logic}
\keyword{manip}
