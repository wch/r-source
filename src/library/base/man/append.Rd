\name{append}
\alias{append}
\title{Vector Merging}
\description{
  Add elements to a vector.
}
\usage{
append(x, values, after=length(x))
}
\arguments{
  \item{x}{the vector to be modified.}
  \item{values}{to be included in the modified vector.}
  \item{after}{a subscript, after which the values are to be appended.}
}
\value{
  A vector containing the values in \code{x} with the elements of
  \code{values} appended after the specified element of \code{x}.
}
\examples{
all(append(1:5, 0:1, after=3) ==
    append(1:3, c(0:1, 4:5)))
}
\keyword{manip}
