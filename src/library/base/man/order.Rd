\name{order}
\title{Ordering Permutation}
\usage{
order(\dots)
}
\alias{order}
\arguments{
\item{\dots}{a sequence of vectors.}
}
\description{
\code{order} returns a permutation which will rearrange its first
argument into ascending order.
In the case of ties in the first vector, values in the second
are used to break the ties.
If the values are still tied, values in the later arguments
are used to break the tie.

\code{NA} values are treated as greater than any other values
so that permutations returned by
\code{order} move \code{NA}
values to the top end of the array.
}
\seealso{
\code{\link{sort}}.
}
\examples{
x <- c(5:1, 6:8, 12:9)
y <- (x - 5)^2
# rearrange matched vectors so that
# the first is in ascending order
o <- order(x)
rbind(x[o], y[o])
}
\keyword{univar}
\keyword{manip}
