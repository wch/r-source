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
   a:b

seq(from, to)
seq(from, to, by= )
seq(from, to, length.out= )
seq(along.with= )
seq(from)
}
\arguments{
  \item{from}{starting value of sequence.}
  \item{to}{(maximal) end value of the sequence.}
  \item{by}{increment of the sequence.}
  \item{length.out}{desired length of the sequence.}
  \item{along.with}{take the length from the length of this argument.}
  \item{a,b}{\code{\link{factor}}s of same length.}
}
\details{
  The binary operator \code{:} has two meanings: for factors \code{a:b} is
  equivalent to \code{\link{interaction}(a, b)} (except for labelling by
  \code{la:lb} not \code{la.lb}).  For numeric arguments \code{a:b} is
  equivalent to \code{seq(from=a, to=b)}.

  The interpretation of the unnamed arguments of \code{seq} is
  \emph{not} standard, and it is recommended always to name the
  arguments when programming.

  Function \code{seq} is generic, and only the default method is
  described here.

  The operator \code{:} and the \code{seq(from, to)} form generate the
  sequence \code{from, from+1, \dots, to}.

  The second form generates \code{from, from+by}, \ldots, up to the
  sequence value less than or equal to \code{to}.

  The third generates a sequence of \code{length.out} equally spaced
  values from \code{from} to \code{to}.

  The fourth form generates the sequence \code{1, 2, \dots, length(along.with)}.

  The last generates the sequence \code{1, 2, \dots, length(from)}
  (as if argument \code{along} had been specified),
  \emph{unless} the argument is numeric of length 1 when it is
  interpreted as \code{1:from} (even for \code{seq(0)} for
  compatibility with S).

  If \code{from} and \code{to} are factors of the same length, then
  \code{from : to} returns the \dQuote{cross} of the two.

  Very small sequences (with \code{from - to} of the order of \eqn{10^{-14}}
  times the larger of the ends) will return \code{from}.
}
\value{
  The result is of \code{mode} \code{"integer"} if \code{from} is
  (numerically equal to an) integer and, e.g., only \code{to} is
  specified, or also if only \code{length} or only \code{along.with} is
  specified.
  % MM: to specify all the conditions doesn't seem worth, nor should the
  % code be changed just for docu.purposes; e.g. str(seq(from=1:1, to=8, by=3:3))
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  The method \code{\link{seq.POSIXt}}.

  \code{\link{rep}},
  \code{\link{sequence}},
  \code{\link{row}},
  \code{\link{col}}.

  As an alternative to using \code{:} for factors, \code{\link{interaction}}.
}
\examples{
1:4
pi:6 # float
6:pi # integer

seq(0,1, length=11)
seq(rnorm(20))
seq(1,9, by = 2) # match
seq(1,9, by = pi)# stay below
seq(1,6, by = 3)
seq(1.575, 5.125, by=0.05)
seq(17) # same as 1:17

for (x in list(NULL, letters[1:6], list(1,pi)))
  cat("x=",deparse(x),";  seq(along = x):",seq(along = x),"\n")

f1 <- gl(2,3); f1
f2 <- gl(3,2); f2
f1:f2 # a factor, the "cross"  f1 x f2
}
\keyword{manip}
