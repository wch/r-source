\name{seq}
\title{Sequence Generation}
\alias{seq}
\alias{seq.default}
\alias{:}
\description{
  Generate regular sequences.
}
\synopsis{
seq(...)
seq.default(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
            length.out = NULL, along.with = NULL, ...)
}
\usage{
from:to
seq(from, to)
seq(from, to, by=)
seq(from, to, length=)
seq(along)
}
\arguments{
  \item{from}{starting value of sequence.}
  \item{to}{(maximal) end value of the sequence.}
  \item{by}{increment of the sequence.}
  \item{length}{desired length of the sequence.}
  \item{along}{take the length from the length of this argument.}
}
\details{
  The interpretation of the unnamed arguments of \code{seq} is
  \emph{not} standard, and it is recommended to always name the
  arguments when programming.
 
  The operator \code{:} and the first \code{seq(.)} form generate the
  sequence \code{from, from+1}, \ldots, \code{to}.
  \code{seq} is a generic function.

  The second form generates \code{from, from+by}, \ldots, \code{to}.

  The third generates a sequence of \code{length} equally spaced values
  from \code{from} to \code{to}.

  The last generates the sequence \code{1, 2}, \ldots,
  \code{length(along)}, unless the argument is of length 1 when it is
  interpreted as a \code{length} argument.

  If \code{from} and \code{to} are factors of the same length, then
  \code{from : to} returns the ``cross'' of the two.

  Very small sequences (with \code{from - to} of the order of 1e-14
  times the larger of the ends) will return \code{from}.
}
\value{
  The result is of \code{mode} \code{"integer"} if \code{from} is
  (numerically equal to an) integer and \code{by} is not specified.
}
\seealso{
  \code{\link{rep}},
  \code{\link{sequence}},
  \code{\link{row}},
  \code{\link{col}}.
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
seq(17) # same as 1:17

for (x in list(NULL, letters[1:6], list(1,pi)))
  cat("x=",deparse(x),";  seq(along = x):",seq(along = x),"\n")

f1 <- gl(2,3); f1
f2 <- gl(3,2); f2
f1:f2 # a factor, the ``cross''  f1 x f2
}
\keyword{manip}
