\name{sort}
\title{Sort a Vector}
\usage{
sort(x, partial = NULL, na.last = NA)
}
\alias{sort}
\description{
  Sort \code{\link{numeric}} or  \code{\link{complex}}  \code{x}
  (partially) into ascending order.
\details{
  If \code{partial} is non \code{NULL}, it is taken to contain indexes
  of elements of \code{x} which are to be placed in their correct
  positions by partial sorting.  After the sort, the values specified in
  \code{partial} are in their correct position in the sorted array.  Any
  values smaller than these values are guaranteed to have a smaller
  index in the sorted array and any values which are greater are
  guaranteed to have a bigger index in the sorted array.

  If \code{na.last} is \code{NA}, \code{sort} removes missing values.
  If \code{na.last} is \code{TRUE}, missing values are put last, if
  \code{FALSE}, missing values are put first.
}
\seealso{
  \code{\link{order}}, \code{\link{rank}}.
}
\examples{
data(swiss)
x <- swiss$Education[1:25]
x; sort(x); sort(x, partial = c(10,15))
median # shows you another example for `partial'
}
\keyword{univar}
\keyword{manip}
\keyword{arith}
