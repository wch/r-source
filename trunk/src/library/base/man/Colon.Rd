% File src/library/base/man/Colon.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{Colon}
\title{Colon Operator}
\alias{:}
\alias{colon}
\description{
  Generate regular sequences.
}
\usage{
from:to
   a:b
}
\arguments{
  \item{from}{starting value of sequence.}
  \item{to}{(maximal) end value of the sequence.}
  \item{a, b}{\code{\link{factor}}s of the same length.}
}
\details{
  The binary operator \code{:} has two meanings: for factors \code{a:b} is
  equivalent to \code{\link{interaction}(a, b)} (but the levels are
  ordered and labelled differently).

  For other arguments \code{from:to} is equivalent to \code{seq(from, to)},
  and generates a sequence from \code{from} to \code{to} in steps of \code{1}
  or \code{-1}.  Value \code{to} will be included if it differs from
  \code{from} by an integer up to a numeric fuzz of about \code{1e-7}.
  Non-numeric arguments are coerced internally (hence without
  dispatching methods) to numeric---complex values will have their
  imaginary parts discarded with a warning.
}
\value{
  For numeric arguments, a numeric vector.  This will be of type
  \code{\link{integer}} if \code{from} is integer-valued and the result
  is representable in the \R integer type, otherwise of type
  \code{"double"} (aka \code{\link{mode}} \code{"\link{numeric}"}).

  For factors, an unordered factor with levels labelled as \code{la:lb}
  and ordered lexicographically (that is, \code{lb} varies fastest).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.\cr
  (for numeric arguments: S does not have \code{:} for factors.)
}
\seealso{
  \code{\link{seq}} (a \emph{generalization} of \code{from:to}).

  As an alternative to using \code{:} for factors, \code{\link{interaction}}.

  For \code{:} used in the formal representation of an interaction, see
  \code{\link{formula}}.
}
\examples{
1:4
pi:6 # real
6:pi # integer

f1 <- gl(2, 3); f1
f2 <- gl(3, 2); f2
f1:f2 # a factor, the "cross"  f1 x f2
}
\keyword{manip}
