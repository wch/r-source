\name{crossprod}
\alias{crossprod}
\alias{tcrossprod}
\title{Matrix Crossproduct}
\description{
  Given matrices \code{x} and \code{y} as arguments, return a matrix
  cross-product.  This is formally equivalent to (but faster than) the
  call \code{t(x) \%*\% y} (\code{crossprod}) or \code{x \%*\% t(y)}
  (\code{tcrossprod}).
}
\usage{
crossprod(x, y = NULL)

tcrossprod(x, y = NULL)
}
\arguments{
  \item{x, y}{matrices: \code{y = NULL} is taken to be the
    same matrix as \code{x}.  Vectors are promoted to single-column
    matrices.}
}
\note{
  When \code{x} or \code{y} are not matrices, they are treated as column or
  row matrices, but their \code{\link{names}} are usually \bold{not}
  promoted to \code{\link{dimnames}}.  Hence, currently, the last
  example has empty dimnames.
  %% Consider using a new optional argument, say 'make.names = NA'
  %% {as in kronecker} with possible values  FALSE / NA / TRUE  where
  %% FALSE gives empty dimnames; NA = current behavior; TRUE gives "as much as sensible"
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
}
\keyword{algebra}
\keyword{array}
