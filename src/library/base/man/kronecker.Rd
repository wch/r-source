% File src/library/base/man/kronecker.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{kronecker}

\alias{kronecker}
\alias{.kronecker} % an internal version.
\alias{\%x\%}

\title{Kronecker Products on Arrays}

\description{
  Computes the generalised kronecker product of two arrays,
  \code{X} and \code{Y}.
}


\usage{
kronecker(X, Y, FUN = "*", make.dimnames = FALSE, \dots)
X \%x\% Y
}

\arguments{
 \item{X}{A vector or array.}
 \item{Y}{A vector or array.}
 \item{FUN}{a function; it may be a quoted string.}
 \item{make.dimnames}{Provide dimnames that are the product of the
   dimnames of \code{X} and \code{Y}.}
 \item{\dots}{optional arguments to be passed to \code{FUN}.}
}

\details{
  If \code{X} and \code{Y} do not have the same number of
  dimensions, the smaller array is padded with dimensions of size
  one.  The returned array comprises submatrices constructed by
  taking \code{X} one term at a time and expanding that term as
  \code{FUN(x, Y, ...)}.

  \code{\%x\%} is an alias for \code{kronecker} (where
  \code{FUN} is hardwired to \code{"*"}).
}

\value{
  An array \code{A} with dimensions \code{dim(X) * dim(Y)}.
}

\references{
  Shayle R. Searle (1982)
  \emph{Matrix Algebra Useful for Statistics.} John Wiley and Sons.
}

\author{Jonathan Rougier}

\seealso{
  \code{\link{outer}}, on which \code{kronecker} is built
  and \code{\link{\%*\%}} for usual matrix multiplication.
}

\examples{
# simple scalar multiplication
( M <- matrix(1:6, ncol = 2) )
kronecker(4, M)
# Block diagonal matrix:
kronecker(diag(1, 3), M)

# ask for dimnames

fred <- matrix(1:12, 3, 4, dimnames = list(LETTERS[1:3], LETTERS[4:7]))
bill <- c("happy" = 100, "sad" = 1000)
kronecker(fred, bill, make.dimnames = TRUE)

bill <- outer(bill, c("cat" = 3, "dog" = 4))
kronecker(fred, bill, make.dimnames = TRUE)
}

\keyword{array}
