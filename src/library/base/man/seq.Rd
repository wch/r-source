% File src/library/base/man/seq.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{seq}
\title{Sequence Generation}
\alias{seq}
\alias{seq.default}
\alias{seq.int}
\alias{seq_along}
\alias{seq_len}
\description{
  Generate regular sequences.  \code{seq} is a standard generic with a
  default method.  \code{seq.int} is a primitive which can be
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
  \item{from, to}{the starting and (maximal) end values of the
    sequence.  Of length \code{1} unless just \code{from} is supplied as
    an unnamed argument.}
  \item{by}{number: increment of the sequence.}
  \item{length.out}{desired length of the sequence.  A
    non-negative number, which for \code{seq} and \code{seq.int} will be
    rounded up if fractional.}
  \item{along.with}{take the length from the length of this argument.}
}
\details{
  Numerical inputs should all be \link{finite} (that is, not infinite,
  \code{\link{NaN}} or \code{NA}).

  The interpretation of the unnamed arguments of \code{seq} and
  \code{seq.int} is \emph{not} standard, and it is recommended always to
  name the arguments when programming.

  \code{seq} is  generic, and only the default method is described here.
  Note that it dispatches on the class of the \strong{first} argument
  irrespective of argument names.  This can have unintended consequences
  if it is called with just one argument intending this to be taken as
  \code{along.with}: it is much better to use \code{seg_along} in that
  case.

  \code{seq.int} is an \link{internal generic} which dispatches on
  methods for \code{"seq"} based on the class of the first supplied
  argument (before argument matching).

  Typical usages are
\preformatted{seq(from, to)
seq(from, to, by= )
seq(from, to, length.out= )
seq(along.with= )
seq(from)
seq(length.out= )
}
  The first form generates the sequence \code{from, from+/-1, \dots, to}
  (identical to \code{from:to}).

  The second form generates \code{from, from+by}, \ldots, up to the
  sequence value less than or equal to \code{to}.  Specifying \code{to -
  from} and \code{by} of opposite signs is an error.  Note that the
  computed final value can go just beyond \code{to} to allow for
  rounding error, but is truncated to \code{to}.  (\sQuote{Just beyond}
  is by up to \eqn{10^{-10}}{1e-10} times \code{abs(from - to)}.)

  The third generates a sequence of \code{length.out} equally spaced
  values from \code{from} to \code{to}.  (\code{length.out} is usually
  abbreviated to \code{length} or \code{len}, and \code{seq_len} is much
  faster.)

  The fourth form generates the integer sequence \code{1, 2, \dots,
    length(along.with)}.  (\code{along.with} is usually abbreviated to
  \code{along}, and \code{seq_along} is much faster.)

  The fifth form generates the sequence \code{1, 2, \dots, length(from)}
  (as if argument \code{along.with} had been specified), \emph{unless}
  the argument is numeric of length 1 when it is interpreted as
  \code{1:from} (even for \code{seq(0)} for compatibility with S).
  Using either \code{seq_along} or \code{seq_len} is much preferred
  (unless strict S compatibility is essential).

  The final form generates the integer sequence \code{1, 2, \dots,
  length.out} unless \code{length.out = 0}, when it generates
  \code{integer(0)}.

  Very small sequences (with \code{from - to} of the order of \eqn{10^{-14}}
  times the larger of the ends) will return \code{from}.

  For \code{seq} (only), up to two of \code{from}, \code{to} and
  \code{by} can be supplied as complex values provided \code{length.out}
  or \code{along.with} is specified.  More generally, the default method
  of \code{seq} will handle classed objects with methods for
  the \code{Math}, \code{Ops} and \code{Summary} group generics.

  \code{seq.int}, \code{seq_along} and \code{seq_len} are
  \link{primitive}.
}
% We used to explain some of the circumstances when integer was
% returned, but people read that carelessly, so no longer.
\value{
  \code{seq.int} and the default method of \code{seq} for numeric
  arguments return a vector of type \code{"integer"} or \code{"double"}:
  programmers should not rely on which.

  \code{seq_along} and \code{seq_len} return an integer vector, unless
  it is a \emph{long vector} when it will be double.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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
seq(0, 1, length.out = 11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17) # same as 1:17, or even better seq_len(17)
}
\keyword{manip}
