\name{sort}
\alias{sort}
\alias{is.unsorted}
\title{Sorting a Vector}
\description{
  Sort a numeric or complex vector (partially) into ascending order.
}
\usage{
sort(x, partial = NULL, na.last = NA)
is.unsorted(x, na.rm = FALSE)
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

  \code{is.unsorted} returns a logical indicating if \code{x} is sorted
  increasingly, i.e. \code{is.unsorted(x)} is true if \code{any(x !=
    sort(x))} (and there are no \code{NA}s).
}
\seealso{
  \code{\link{order}}, \code{\link{rank}}.
}
\examples{
data(swiss)
x <- swiss$Education[1:25]
x; sort(x); sort(x, partial = c(10, 15))
median # shows you another example for `partial'

stopifnot(!is.unsorted(sort(x)),
          !is.unsorted(LETTERS),
           is.unsorted(c(NA,1:3,2), na.rm = TRUE))
}
\keyword{univar}
\keyword{manip}
\keyword{arith}
