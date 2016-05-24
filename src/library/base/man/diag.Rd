% File src/library/base/man/diag.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{diag}
\title{Matrix Diagonals}
\usage{
diag(x = 1, nrow, ncol)
diag(x) <- value
}
\alias{diag}
\alias{diag<-}
\description{
  Extract or replace the diagonal of a matrix,
  or construct a diagonal matrix.
}
\arguments{
  \item{x}{a matrix, vector or 1D \code{\link{array}}, or missing.}
  \item{nrow, ncol}{optional dimensions for the result when \code{x} is
    not a matrix.}
  \item{value}{either a single value or a vector of length equal to that
    of the current diagonal.  Should be of a mode which can be coerced
    to that of \code{x}.}
}
\details{
  \code{diag} has four distinct usages:
  \enumerate{
    \item \code{x} is a matrix, when it extracts the diagonal.
    \item \code{x} is missing and \code{nrow} is specified, it returns
    an identity matrix.
    \item \code{x} is a scalar (length-one vector) and the only
    argument, it returns a square identity matrix of size given by the scalar.
    \item \code{x} is a \sQuote{numeric} (\code{\link{complex}},
    \code{numeric}, \code{integer}, \code{\link{logical}}, or
    \code{\link{raw}}) vector, either of length at least 2 or there
    were further arguments.  This returns a matrix with the given
    diagonal and zero off-diagonal entries.
  }
  It is an error to specify \code{nrow} or \code{ncol} in the first case.
}
\value{
  If \code{x} is a matrix then \code{diag(x)} returns the diagonal of
  \code{x}.  The resulting vector will have \code{\link{names}} if the
  matrix \code{x} has matching column and rownames.

  The replacement form sets the diagonal of the matrix \code{x} to the
  given value(s).

  In all other cases the value is a diagonal matrix with \code{nrow}
  rows and \code{ncol} columns (if \code{ncol} is not given the matrix
  is square).  Here \code{nrow} is taken from the argument if specified,
  otherwise inferred from \code{x}: if that is a vector (or 1D array) of
  length two or more, then its length is the number of rows, but if it
  is of length one and neither \code{nrow} nor \code{ncol} is specified,
  \code{nrow = as.integer(x)}.

  When a diagonal matrix is returned, the diagonal elements are one
  except in the fourth case, when \code{x} gives the diagonal elements:
  it will be recycled or truncated as needed, but fractional recycling
  and truncation will give a warning.
}
\note{
  Using \code{diag(x)} can have unexpected effects if \code{x} is a
  vector that could be of length one.  Use \code{diag(x, nrow =
    length(x))} for consistent behaviour.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
\code{\link{upper.tri}}, \code{\link{lower.tri}}, \code{\link{matrix}}.
}
\examples{
dim(diag(3))
diag(10, 3, 4) # guess what?
all(diag(1:3) == {m <- matrix(0,3,3); diag(m) <- 1:3; m})

## other "numeric"-like diagonal matrices :
diag(c(1i,2i))    # complex
diag(TRUE, 3)     # logical
diag(as.raw(1:3)) # raw
(D2 <- diag(2:1, 4)); typeof(D2) # "integer"

require(stats)
## diag(<var-cov-matrix>) = variances
diag(var(M <- cbind(X = 1:5, Y = rnorm(5))))
#-> vector with names "X" and "Y"
rownames(M) <- c(colnames(M), rep("", 3));
M; diag(M) #  named as well
}
\keyword{array}
