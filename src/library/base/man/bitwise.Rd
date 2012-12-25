% File src/library/base/man/octmode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012 R Core Team
% Distributed under GPL 2 or later

\name{bitwise}
\alias{bitwNot}
\alias{bitwAnd}
\alias{bitwOr}
\alias{bitwXor}
\alias{bitwShiftL}
\alias{bitwShiftR}

\title{Bitwise Logical Operations}
\description{
  Logical operations on integer vectors with elements viewed as sets of bits.
}
\usage{
bitwNot(a)
bitwAnd(a, b)
bitwOr(a, b)
bitwXor(a, b)

bitwShiftL(a, n)
bitwShiftR(a, n)
}
\arguments{
  \item{a, b}{integer vectors; numeric vectors are coerced to integer vectors.}
  \item{n}{non-negative integer vector of values up to 31.}
}
\details{
  Each element of an integer vector has 32 bits.

  Pairwise operations can result in integer \code{NA}.

  Shifting is done assuming the values represent unsigned integers.
}
\value{
  An integer vector of length the longer of the arguments, or zero
  length if one is zero-length.

  The output element is \code{NA} if an input is \code{NA} (after
  coercion) or an invalid shift.
}
\seealso{
  The logical operators, \code{\link{!}},  \code{\link{&}},
  \code{\link{|}}, \code{\link{xor}}.

  The classes \code{"octmode"} and \code{"hexmnode"} whose
  implementation of the standard logical operators is based on these
  functions.

  Package \pkg{bitOps} has similar functions for numeric vectors which
  differ in the way they treat integers \eqn{2^{31}}{2^31} or larger.
}

\examples{
bitwAnd(15L, 7L)
bitwOr(15L, 7L)
bitwXor(15L, 7L)
bitwXor(-1L, 1L)

bitwShiftR(-1, 1:31) # shifts of 2^32-1 = 4294967295
}

\keyword{logic}
