\name{seq}
\title{Sequence Generation}
\alias{seq}
\alias{seq.default}
\alias{seq.int}
\alias{seq_along}
\alias{seq_len}
\description{
  Generate regular sequences.  \code{seq} is a standard generic with a
  default method.  \code{seq.int} is an internal generic which can be
  much faster but has a few restrictions.  \code{seq_along} and
  \code{seq_len} are very fast primitives for two common cases.
}
\usage{
seq(\dots)

\method{seq}{default}(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
    length.out = NULL, along.with = NULL, \dots)

seq.int(from, to, by, length.out, along.with, \dots)

seq_along(along.with)
seq_len(length.out)
}
\arguments{
  \item{\dots}{arguments passed to or from methods.}
  \item{from, to}{the starting and (maximal) end value of the sequence.}
  \item{by}{number: increment of the sequence.}
  \item{length.out}{desired length of the sequence.  A
    non-negative number, which for \code{seq} and \code{seq.int} will be
    rounded up if fractional.}
  \item{along.with}{take the length from the length of this argument.}
}
\details{
  The interpretation of the unnamed arguments of \code{seq} and
  \code{seq.int} is \emph{not} standard, and it is recommended always to
  name the arguments when programming.

  Both \code{seq} are \code{seq.int} are generic, and only the default
  method is described here.  Typical usages are
\preformatted{
seq(from, to)
seq(from, to, by= )
seq(from, to, length.out= )
seq(along.with= )
seq(from)
seq(length.out= )
}
  The first form generates the sequence \code{from, from+/-1, \dots, to}
  (identical to \code{from:to}).

  The second form generates \code{from, from+by}, \ldots, up to the
  sequence value less than or equal to \code{to}.  Specifying
  \code{to - from} and \code{by} of opposite signs is an error.

  The third generates a sequence of \code{length.out} equally spaced
  values from \code{from} to \code{to}.  (\code{length.out} is usually
  abbreviated to \code{length} or \code{len}, and \code{seq_len} is much
  faster.)

  The fourth form generates the sequence \code{1, 2, \dots,
    length(along.with)}.  (\code{along.with} is usually abbreviated to
  \code{along}, and \code{seq_along} is much faster.) 

  The fifth form generates the sequence \code{1, 2, \dots, length(from)}
  (as if argument \code{along.with} had been specified),
  \emph{unless} the argument is numeric of length 1 when it is
  interpreted as \code{1:from} (even for \code{seq(0)} for
  compatibility with S).

  The final form generates \code{1, 2, \dots, length.out} unless
  \code{length.out = 0}, when it generates \code{integer(0)}.

  Very small sequences (with \code{from - to} of the order of \eqn{10^{-14}}
  times the larger of the ends) will return \code{from}.

  For \code{seq}(only), up to two of \code{from}, \code{to} and \code{by} can
  be supplied as complex values provided \code{length.out} or
  \code{along.with} is specified.
}
\value{
  Currently, the default method returns a result of type
  \code{"integer"} if \code{from} is (numerically equal to an)
  integer and, e.g., only \code{to} is specified, or also if only
  \code{length} or only \code{along.with} is specified.  \strong{Note:}
  this may change in the future and programmers should not rely on it.
  % MM: to specify all the conditions doesn't seem worth, nor should the
  % code be changed just for docu.purposes; e.g. str(seq(from=1:1, to=8, by=3:3))
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  The methods \code{\link{seq.Date}} and \code{\link{seq.POSIXt}}.

  \code{\link{:}},
  \code{\link{rep}},
  \code{\link{sequence}},
  \code{\link{row}},
  \code{\link{col}}.
}
\examples{
seq(0, 1, length=11)
seq(rnorm(20))
seq(1, 9, by = 2) # match
seq(1, 9, by = pi)# stay below
seq(1, 6, by = 3)
seq(1.575, 5.125, by=0.05)
seq(17) # same as 1:17
}
\keyword{manip}
