\name{seq}
\title{Sequence Generation}
\usage{
seq(from, to)
seq(from, to, by=)
seq(from, to, length=)
seq(along)
}
\alias{seq}
\description{
The first form above generates the sequence
\code{from, from+1}, \ldots, \code{to}.
The second,
\code{from, from+by}, \ldots, \code{to}.
The third generates a sequence of
\code{length}
equally spaced values from
\code{from} to \code{to}.
The last generates the sequence
\code{1, 2}, \ldots, \code{length(along)}.
}
\seealso{
\code{\link{rep}}, \code{\link{sequence}}, \code{\link{row}}, \code{\link{col}}.
}
\keyword{manip}
