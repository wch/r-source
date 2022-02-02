% File src/library/base/man/octmode.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016-2017 R Core Team
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
  Notably these \emph{do} work bitwise for \code{\link{raw}} arguments.

  The classes \code{"\link{octmode}"} and \code{"\link{hexmode}"} whose
  implementation of the standard logical operators is based on these
  functions.

  Package \CRANpkg{bitops} has similar functions for numeric vectors which
  differ in the way they treat integers \eqn{2^{31}}{2^31} or larger.
}

\examples{
bitwNot(0:12) # -1 -2  ... -13
bitwAnd(15L, 7L) #  7
bitwOr (15L, 7L) # 15
bitwXor(15L, 7L) #  8
bitwXor(-1L, 1L) # -2

## The "same" for 'raw' instead of integer :
rr12 <- as.raw(0:12) ; rbind(rr12, !rr12)
c(r15 <- as.raw(15), r7 <- as.raw(7)) #  0f 07
r15 & r7    # 07
r15 | r7    # 0f
xor(r15, r7)# 08

bitwShiftR(-1, 1:31) # shifts of 2^32-1 = 4294967295
}

\keyword{logic}
