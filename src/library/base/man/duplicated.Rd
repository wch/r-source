\name{duplicated}
\alias{duplicated}
\alias{duplicated.default}
\alias{duplicated.data.frame}
\title{Determine Duplicate Elements}
\usage{
duplicated(x, incomparables = FALSE)
}
\description{
  Determines which elements of a vector of data frame are duplicates
  of elements with smaller subscripts, and returns a logical vector
  indicating which elements (rows) are duplicates.
}
\arguments{
  \item{x}{an atomic vector or a data fram.e}
  \item{incomparables}{a vector of values that cannot be compared.
    Currently, \code{FALSE} is the only possible value, meaning that all
    values can be compared.}
}
\details{
  This is a generic function with methods for vectors and data frames.
  The data frame method works by pasting together a character
  representation of the rows separated by \code{\r}, so may be imperfect
  if the data frame has characters with embedded carriage returns or
  columns which do not reliably map to characters.
}
\seealso{\code{\link{unique}}.}
\examples{
x <- c(9:20, 1:5, 3:7, 0:8)
## extract unique elements
(xu <- x[!duplicated(x)])
stopifnot(xu == unique(x), # but unique(x) is more efficient
          0:20 == sort(x[!duplicated(x)]))

data(iris)
stopifnot(duplicated(iris)[143] == TRUE)
}
\keyword{logic}
\keyword{manip}
