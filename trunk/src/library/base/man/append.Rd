\name{append}
\alias{append}
\title{Vector Merging}
\description{
  Add elements to a vector.
}
\usage{
append(x, values, after = length(x))
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
append(1:5, 0:1, after=3)
}
\keyword{manip}
\concept{insert}
