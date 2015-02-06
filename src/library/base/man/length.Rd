% File src/library/base/man/length.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{length}
\alias{length}
\alias{length<-}
\alias{length<-.factor}
\title{Length of an Object}
\description{
  Get or set the length of vectors (including lists) and factors, and of
  any other \R object for which a method has been defined.
}
\usage{
length(x)
length(x) <- value
}
\arguments{
  \item{x}{an \R object.  For replacement, a vector or factor.}
  \item{value}{a non-negative integer or double (which will be rounded down).}
}
\details{
  Both functions are generic: you can write methods to handle specific
  classes of objects, see \link{InternalMethods}.  \code{length<-} has a
  \code{"factor"} method.

  The replacement form can be used to reset the length of a vector.  If
  a vector is shortened, extra values are discarded and when a vector is
  lengthened, it is padded out to its new length with \code{\link{NA}}s
  (\code{nul} for raw vectors).

  Both are \link{primitive} functions.
}

\value{
  The default method for \code{length} currently returns a non-negative
  \code{\link{integer}} of length 1, except for vectors of more than
  \eqn{2^{31}-1}{2^31 - 1} elements, when it returns a double.

  For vectors (including lists) and factors the length is the number of
  elements.  For an environment it is the number of objects in the
  environment, and \code{NULL} has length 0.  For expressions and
  pairlists (including \link{language objects} and dotlists) it is the
  length of the pairlist chain.  All other objects (including functions)
  have length one: note that for functions this differs from S.

  The replacement form removes all the attributes of \code{x} except its
  names, which are adjusted (and if necessary extended by \code{""}).
}

\section{Warning}{
  Package authors have written methods that return a result of length
  other than one (\CRANpkg{Formula}) and that return a vector of type
  \code{\link{double}} (\CRANpkg{Matrix}), even with non-integer values
  (earlier versions of \CRANpkg{sets}). Where a single double value is
  returned that can be represented as an integer it is returned as a
  length-one integer vector.

  As from \R 3.0.0, lengths can be returned as double in base \R.
}

\seealso{\code{nchar} for counting the number of characters in character
  vectors, \code{\link{lengths}} for getting the length of every element
  in a list.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
length(diag(4))  # = 16 (4 x 4)
length(options())  # 12 or more
length(y ~ x1 + x2 + x3)  # 3
length(expression(x, {y <- x^2; y+2}, x^y))  # 3

## from example(warpbreaks)
require(stats)

fm1 <- lm(breaks ~ wool * tension, data = warpbreaks)
length(fm1$call)      # 3, lm() and two arguments.
length(formula(fm1))  # 3, ~ lhs rhs
}
\keyword{attribute}

