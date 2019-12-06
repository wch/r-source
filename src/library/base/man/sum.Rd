% File src/library/base/man/sum.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{sum}
\alias{sum}
\title{Sum of Vector Elements}
\description{
  \code{sum} returns the sum of all the values
  present in its arguments.
}
\usage{
sum(\dots, na.rm = FALSE)
}
\arguments{
  \item{\dots}{numeric or complex or logical vectors.}
  \item{na.rm}{logical.  Should missing values (including \code{NaN}) be
    removed?}
}
\value{
  The sum.  If all of the \code{\dots} arguments are of type
  integer or logical, then the sum is \code{\link{integer}} when
  possible and is \code{double} otherwise.  Integer overflow should no
  longer happen since \R version 3.5.0.
  For other argument types it is a length-one numeric
  (\code{\link{double}}) or complex vector.

  \strong{NB:} the sum of an empty set is zero, by definition.
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link[=S3groupGeneric]{Summary}} group generic.
  For this to work properly, the arguments \code{\dots} should be
  unnamed, and dispatch is on the first argument.

  If \code{na.rm} is \code{FALSE} an \code{NA} or \code{NaN} value in
  any of the arguments will cause a value of \code{NA} or \code{NaN} to
  be returned, otherwise \code{NA} and \code{NaN} values are ignored.

  Logical true values are regarded as one, false values as zero.
  For historical reasons, \code{NULL} is accepted and treated as if it
  were \code{integer(0)}.

  Loss of accuracy can occur when summing values of different signs:
  this can even occur for sufficiently long integer inputs if the
  partial sums would cause integer overflow.  Where possible
  extended-precision accumulators are used, typically well supported
  with C99 and newer, but possibly platform-dependent.
}
\section{S4 methods}{
  This is part of the S4 \code{\link[=S4groupGeneric]{Summary}}
  group generic.  Methods for it must use the signature
  \code{x, \dots, na.rm}.

  \sQuote{\link{plotmath}} for the use of \code{sum} in plot annotation.
}
\seealso{
  \code{\link{colSums}} for row and column sums.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{% for beginners
## Pass a vector to sum, and it will add the elements together.
sum(1:5)

## Pass several numbers to sum, and it also adds the elements.
sum(1, 2, 3, 4, 5)

## In fact, you can pass vectors into several arguments, and everything gets added.
sum(1:2, 3:5)

## If there are missing values, the sum is unknown, i.e., also missing, ....
sum(1:5, NA)
## ... unless  we exclude missing values explicitly:
sum(1:5, NA, na.rm = TRUE)
}
\keyword{arith}
