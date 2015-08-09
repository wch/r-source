% File src/library/base/man/is.finite.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{is.finite}
\title{Finite, Infinite and NaN Numbers}
\alias{is.finite}
\alias{is.infinite}
\alias{Inf}
\alias{NaN}
\alias{is.nan}
\alias{finite} % for when people ask what 'finite' means.
\alias{infinite}
\description{
  \code{is.finite} and \code{is.infinite} return a vector of the same
  length as \code{x}, indicating which elements are finite (not infinite
  and not missing) or infinite.

  \code{Inf} and \code{-Inf} are positive and negative infinity
  whereas \code{NaN} means \sQuote{Not a Number}.  (These apply to numeric
  values and real and imaginary parts of complex values but not to
  values of integer vectors.)  \code{Inf} and \code{NaN} are
  \link{reserved} words in the \R language.
}
\usage{
is.finite(x)
is.infinite(x)
is.nan(x)

Inf
NaN
}
\arguments{
  \item{x}{\R object to be tested: the default methods handle atomic
    vectors.}
}
\details{
  \code{is.finite} returns a vector of the same length as \code{x} the
  jth element of which is \code{TRUE} if \code{x[j]} is finite (i.e., it
  is not one of the values \code{NA}, \code{NaN}, \code{Inf} or
  \code{-Inf}) and \code{FALSE} otherwise.  Complex
  numbers are finite if both the real and imaginary parts are.

  \code{is.infinite} returns a vector of the same length as \code{x} the
  jth element of which is \code{TRUE} if \code{x[j]} is infinite (i.e.,
  equal to one of \code{Inf} or \code{-Inf}) and \code{FALSE}
  otherwise.  This will be false unless \code{x} is numeric or complex.
  Complex numbers are infinite if either the real or the imaginary part is.

  \code{is.nan} tests if a numeric value is \code{NaN}.  Do not test
  equality to \code{NaN}, or even use \code{\link{identical}}, since
  systems typically have many different NaN values.  One of these is
  used for the numeric missing value \code{NA}, and \code{is.nan} is
  false for that value.  A complex number is regarded as \code{NaN} if
  either the real or imaginary part is \code{NaN} but not \code{NA}.
  All elements of logical, integer and raw vectors are considered not to
  be NaN.

  All three functions accept \code{NULL} as input and return a length
  zero result. The default methods accept character and raw vectors, and
  return \code{FALSE} for all entries. Prior to \R version 2.14.0 they
  accepted all input, returning \code{FALSE} for most non-numeric
  values; cases which are not atomic vectors are now signalled as
  errors.

  All three functions are generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\note{
  In \R, basically all mathematical functions (including basic
  \code{\link{Arithmetic}}), are supposed to work properly with
  \code{+/- Inf} and \code{NaN} as input or output.

  The basic rule should be that calls and relations with \code{Inf}s
  really are statements with a proper mathematical \emph{limit}.

  Computations involving \code{NaN} will return \code{NaN} or perhaps
  \code{\link{NA}}: which of those two is not guaranteed and may depend
  on the \R platform (since compilers may re-order computations).
}
\value{
  A logical vector of the same length as \code{x}: \code{dim},
  \code{dimnames} and \code{names} attributes are preserved.
}
\seealso{
  \code{\link{NA}}, \sQuote{\emph{Not Available}} which is not a number
  as well, however usually used for missing values and applies to many
  modes, not just numeric and complex.

  \code{\link{Arithmetic}}, \code{\link{double}}.
}
\references{
  The IEC 60559 standard, also known as the
  ANSI/IEEE 754 Floating-Point Standard.

  \url{https://en.wikipedia.org/wiki/NaN}.

  D. Goldberg (1991) \emph{What Every Computer Scientist Should Know
    about Floating-Point Arithmetic}  ACM Computing Surveys, \bold{23(1)}.\cr
  Postscript version available at
  \url{http://www.validlab.com/goldberg/paper.ps}
  Extended PDF version at \url{http://www.validlab.com/goldberg/paper.pdf}

  The C99 function \code{isfinite} is used for \code{is.finite} if available.
}
\examples{
pi / 0 ## = Inf a non-zero number divided by zero creates infinity
0 / 0  ## =  NaN

1/0 + 1/0 # Inf
1/0 - 1/0 # NaN

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
