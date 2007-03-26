\name{is.finite}
\title{Finite, Infinite and NaN Numbers}
\alias{is.finite}
\alias{is.infinite}
\alias{Inf}
\alias{NaN}
\alias{is.nan}
\alias{finite} % for when people ask what 'finite' means.
\description{
  \code{is.finite} and \code{is.infinite} return a vector of the same
  length as \code{x}, indicating which elements are finite (not infinite
  and not missing).

  \code{Inf} and \code{-Inf} are positive and negative \dQuote{infinity}
  whereas \code{NaN} means \dQuote{Not a Number}.  (These apply to numeric
  values and real and imaginary parts of complex values but not to
  values of integer vectors.)
}
\usage{
is.finite(x)
is.infinite(x)
Inf
NaN
is.nan(x)
}
\arguments{
  \item{x}{(numerical) object to be tested.}
}
\details{
  \code{is.finite} returns a vector of the same length as \code{x} the
  jth element of which is \code{TRUE} if \code{x[j]} is finite (i.e., it
  is not one of the values \code{NA}, \code{NaN}, \code{Inf} or
  \code{-Inf}).  All elements of types other than logical, integer,
  numeric and complex vectors are false.  Complex numbers are finite if
  both the real and imaginary parts are.

  \code{is.infinite} returns a vector of the same length as \code{x}
  the jth element of which is \code{TRUE} if \code{x[j]} is
  infinite (i.e., equal to one of \code{Inf} or \code{-Inf}). This will
  be false unless \code{x} is numeric or complex.  Complex numbers are
  infinite if either the real and imaginary part is.

  \code{is.nan} tests if a numeric value is \code{NaN}.  Do not test
  equality to \code{NaN}, or even use \code{\link{identical}}, since
  systems typically have many different NaN values.  One of these is used
  for the numeric missing value \code{NA}, and \code{is.nan} is false
  for that value.  It is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.  The default
  method handles real and complex vectors: a complex number is regarded
  as \code{NaN} if either the real or imaginary part is \code{NaN} but
  not \code{NA}.
}
\note{
  In \R, basically all mathematical functions (including basic
  \code{\link{Arithmetic}}), are supposed to work properly with
  \code{+/- Inf} and \code{NaN} as input or output.

  The basic rule should be that calls and relations with \code{Inf}s
  really are statements with a proper mathematical \emph{limit}.
}
\seealso{
  \code{\link{NA}}, \sQuote{\emph{Not Available}} which is not a number
  as well, however usually used for missing values and applies to many
  modes, not just numeric.
}
\references{
  The IEC 60559 standard, also know as the
  ANSI/IEEE 754 Floating-Point Standard.

  D. Goldberg (1991) \emph{What Every Computer Scientist Should Know
    about Floating-Point Arithmetic}  ACM Computing Surveys, \bold{23(1)}.\cr
  Postscript version available at
  \url{http://www.validlab.com/goldberg/paper.ps}
  Extended PDF version at \url{http://www.validlab.com/goldberg/paper.pdf}
  
  \url{http://grouper.ieee.org/groups/754/} for accessible information.

  The C99 function \code{isfinite} is used for \code{is.finite} if available.
}
\examples{
pi / 0 ## = Inf a non-zero number divided by zero creates infinity
0 / 0  ## =  NaN

1/0 + 1/0# Inf
1/0 - 1/0# NaN

stopifnot(
    1/0 == Inf,
    1/Inf == 0
)
sin(Inf)
cos(Inf)
tan(Inf)
}
\keyword{programming}
\keyword{math}
