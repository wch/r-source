\name{seq}
\title{Sequence Generation}
\usage{
from:to
seq(from, to)
seq(from, to, by=)
seq(from, to, length=)
seq(along)
}
\alias{seq}
\alias{seq.default}
\alias{:}
\description{
The \code{:} and the first \code{seq(.)} form above generate the sequence
\code{from, from+1}, \ldots, \code{to}.
\code{seq} is a generic function.

The second, \code{from, from+by}, \ldots, \code{to}.
The third generates a sequence of \code{length} equally spaced values from
\code{from} to \code{to}.
The last generates the sequence
\code{1, 2}, \ldots, \code{length(along)}.

If \code{from} and \code{to} are factors of the same length,
the ``cross'' of the two is returned.
}
\value{
  The result is of \code{mode} \code{"integer"} if \code{from} is
  (numerically equal to an) integer.
}
\seealso{
\code{\link{rep}}, \code{\link{sequence}}, \code{\link{row}}, \code{\link{col}}.
}
\examples{
1:4
pi:6 # float
6:pi # integer

seq(0,1, length=11)
str(seq(rnorm(20)))
seq(1,9, by = 2) # match
seq(1,9, by = pi)# stay below
seq(1,6, by = 3)
seq(1.575, 5.125, by=0.05)
\testonly{
 3 == seq(3,3,  by=pi)
 3 == seq(3,3.1,by=pi)
 all(seq(1,6,by=3) == c(1,4))
 all(seq(10,4.05,by=-3) == c(10,7))
}

for (x in list(NULL, letters[1:6], list(1,pi)))
  cat("x=",deparse(x),";  seq(along = x):",seq(along = x),"\n")

f1_ gl(2,3); f1
f2_ gl(3,2); f2
f1:f2 # a factor, the ``cross''  f1 x f2
}
\keyword{manip}
