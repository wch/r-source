\name{rle}
\alias{rle}
\title{Run Length Encoding}
\description{
  Compute the lengths and values of runs of equal values in a vector.
}
\usage{
rle(x)
}
\arguments{
  \item{x}{a (numerical, logical or character) vector.}
}
\value{
  A list with components
  \item{lengths}{a vector containing the length of each run.}
  \item{values}{a vector of the same length as \code{lengths} with the
    corresponding values.}
}
\examples{
x <- rev(rep(6:10, 1:5))
rle(x)
## $lengths
## [1] 5 4 3 2 1
## $values
## [1] 10  9  8  7  6
z <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
rle(z)
rle(as.character(z))
}
\keyword{manip}
