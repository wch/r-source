\name{sort}
\alias{sort}
\title{Sort a Vector}
\description{
  Sort a numeric or complex vector (partially) into ascending order.
}
\usage{
sort(x, partial = NULL, na.last = NA)
}
\arguments{
  \item{x}{a numeric or complex vector.}
  \item{partial}{a vector of indices for partial sorting.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
}
\details{
  If \code{partial} is non \code{NULL}, it is taken to contain indexes
  of elements of \code{x} which are to be placed in their correct
  positions by partial sorting.  After the sort, the values specified in
  \code{partial} are in their correct position in the sorted array.  Any
  values smaller than these values are guaranteed to have a smaller
  index in the sorted array and any values which are greater are
  guaranteed to have a bigger index in the sorted array.
}
\seealso{
  \code{\link{order}}, \code{\link{rank}}.
}
\examples{
data(swiss)
x <- swiss$Education[1:25]
x; sort(x); sort(x, partial = c(10, 15))
median # shows you another example for `partial'
}
\keyword{univar}
\keyword{manip}
\keyword{arith}
