% File src/library/base/man/Round.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{Round}
\alias{ceiling}
\alias{floor}
\alias{round}
\alias{signif}
\alias{trunc}
\title{Rounding of Numbers}
\usage{
ceiling(x)
floor(x)
trunc(x, \dots)

round(x, digits = 0)
signif(x, digits = 6)
}
\description{
  \code{ceiling} takes a single numeric argument \code{x} and returns a
  numeric vector containing the smallest integers not less than the
  corresponding elements of \code{x}.

  \code{floor} takes a single numeric argument \code{x} and returns a
  numeric vector containing the largest integers not greater than the
  corresponding elements of \code{x}.

  \code{trunc} takes a single numeric argument \code{x} and returns a
  numeric vector containing the integers formed by truncating the values in
  \code{x} toward \code{0}.

  \code{round} rounds the values in its first argument to the specified
  number of decimal places (default 0).

  \code{signif} rounds the values in its first argument to the specified
  number of significant digits.
}
\arguments{
  \item{x}{a numeric vector.  Or, for \code{round} and \code{signif}, a
    complex vector.}

  \item{digits}{integer indicating the number of decimal places
    (\code{round}) or significant digits (\code{signif}) to be used.
    Negative values are allowed (see \sQuote{Details}).}

  \item{\dots}{arguments to be passed to methods.}
}
\details{
  These are generic functions: methods can be defined for them
  individually or via the \code{\link[=S3groupGeneric]{Math}} group
  generic.

  Note that for rounding off a 5, the IEC 60559 standard is expected to
  be used, \sQuote{\emph{go to the even digit}}.
  Therefore \code{round(0.5)} is \code{0} and \code{round(-1.5)} is
  \code{-2}.  However, this is dependent on OS services and on
  representation error (since e.g.\sspace{}\code{0.15} is not represented
  exactly, the rounding rule applies to the represented number and not
  to the printed number, and so \code{round(0.15, 1)} could be either
  \code{0.1} or \code{0.2}).

  Rounding to a negative number of digits means rounding to a power of
  ten, so for example \code{round(x, digits = -2)} rounds to the nearest
  hundred.

  For \code{signif} the recognized values of \code{digits} are
  \code{1...22}, and non-missing values are rounded to the nearest
  integer in that range.  Complex numbers are rounded to retain the
  specified number of digits in the larger of the components.  Each
  element of the vector is rounded individually, unlike printing.

  These are all primitive functions.
}
\section{S4 methods}{
  These are all (internally) S4 generic.

  \code{ceiling}, \code{floor} and \code{trunc} are members of the
  \code{\link[=S4groupGeneric]{Math}} group generic.  As an S4
  generic, \code{trunc} has only one argument.

  \code{round} and \code{signif} are members of the
  \code{\link[=S4groupGeneric]{Math2}} group generic.
}
\section{Warning}{
  The realities of computer arithmetic can cause unexpected results,
  especially with \code{floor} and \code{ceiling}.  For example, we
  \sQuote{know} that \code{floor(log(x, base = 8))} for \code{x = 8} is
  \code{1}, but \code{0} has been seen on an \R platform.  It is
  normally necessary to use a tolerance.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{as.integer}}.
}
\examples{
round(.5 + -2:4) # IEEE rounding: -2  0  0  2  2  4  4
( x1 <- seq(-2, 4, by = .5) )
round(x1) #-- IEEE rounding !
x1[trunc(x1) != floor(x1)]
x1[round(x1) != floor(x1 + .5)]
(non.int <- ceiling(x1) != floor(x1))

x2 <- pi * 100^(-1:3)
round(x2, 3)
signif(x2, 3)
}
\keyword{arith}
