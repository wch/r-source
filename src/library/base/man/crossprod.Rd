% File src/library/base/man/crossprod.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{crossprod}
\alias{crossprod}
\alias{tcrossprod}
\title{Matrix Crossproduct}
\description{
  Given matrices \code{x} and \code{y} as arguments, return a matrix
  cross-product.  This is formally equivalent to (but usually slightly
  faster than) the call \code{t(x) \%*\% y} (\code{crossprod}) or
  \code{x \%*\% t(y)} (\code{tcrossprod}).
}
\usage{
crossprod(x, y = NULL)

tcrossprod(x, y = NULL)
}
\arguments{
  \item{x, y}{numeric or complex matrices (or vectors): \code{y = NULL}
    is taken to be the same matrix as \code{x}.  Vectors are promoted to
    single-column or single-row matrices, depending on the context.}
}
\value{
  A double or complex matrix, with appropriate \code{dimnames} taken
  from \code{x} and \code{y}.
}
\note{
  When \code{x} or \code{y} are not matrices, they are treated as column or
  row matrices, but their \code{\link{names}} are usually \bold{not}
  promoted to \code{\link{dimnames}}.  Hence, currently, the last
  example has empty dimnames.

  In the same situation, these matrix products (also \code{\link{\%*\%}})
  are more flexible in promotion of vectors to row or column matrices, such
  that more cases are allowed, since \R 3.2.0.
}
%% Consider using a new optional argument, say 'make.names = NA'
%% {as in kronecker} with possible values  FALSE / NA / TRUE  where
%% FALSE gives empty dimnames; NA = current behavior; TRUE gives "as much as sensible"

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{\%*\%}} and outer product \code{\link{\%o\%}}.
}
\examples{
(z <- crossprod(1:4))    # = sum(1 + 2^2 + 3^2 + 4^2)
drop(z)                  # scalar
x <- 1:4; names(x) <- letters[1:4]; x
tcrossprod(as.matrix(x)) # is
identical(tcrossprod(as.matrix(x)),
          crossprod(t(x)))
tcrossprod(x)            # no dimnames

m <- matrix(1:6, 2,3) ; v <- 1:3; v2 <- 2:1
stopifnot(identical(tcrossprod(v, m), v \%*\% t(m)),
          identical(tcrossprod(v, m), crossprod(v, t(m))),
          identical(crossprod(m, v2), t(m) \%*\% v2))
}
\keyword{algebra}
\keyword{array}
