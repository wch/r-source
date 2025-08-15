% File src/library/stats/man/varimax.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{varimax}
\alias{promax}
\alias{varimax}
\title{Rotation Methods for Factor Analysis}
\description{
  These functions \sQuote{rotate} loading matrices in factor analysis.
}
\usage{
varimax(x, normalize = TRUE, eps = 1e-5)
promax(x, m = 4)
}
\arguments{
  \item{x}{A loadings matrix, with \eqn{p} rows and \eqn{k < p} columns}
  \item{m}{The power used the target for \code{promax}.  Values of 2 to
    4 are recommended.}
  \item{normalize}{logical. Should Kaiser normalization be performed?
    If so the rows of \code{x} are re-scaled to unit length before
    rotation, and scaled back afterwards.}
  \item{eps}{The tolerance for stopping: the relative change in the sum
    of singular values.}
}
\details{
  These seek a \sQuote{rotation} of the factors \code{x \%*\% T} that
  aims to clarify the structure of the loadings matrix.  The matrix
  \code{T} is a rotation (possibly with reflection) for \code{varimax},
  but a general linear transformation for \code{promax}, with the
  variance of the factors being preserved.
}
\value{
  A list with components
  \item{loadings}{The \sQuote{rotated} loadings matrix,
    \code{x \%*\% rotmat}, of class \code{"loadings"}.}
  \item{rotmat}{The \sQuote{rotation} matrix.}
}
\references{
  \bibshow{R:Hendrickson+White:1964,
    R:Kaiser:1958}

  Horst, P. (1965).
  \emph{Factor Analysis of Data Matrices}.
  Holt, Rinehart and Winston.
  Chapter 10.

  Lawley, D. N. and Maxwell, A. E. (1971).
  \emph{Factor Analysis as a Statistical Method}, second edition.
  Butterworths.
}
\seealso{
  \code{\link{factanal}},
  \code{\link{Harman74.cor}}.
}
\examples{
## varimax with normalize = TRUE is the default
fa <- factanal( ~., 2, data = swiss)
varimax(loadings(fa), normalize = FALSE)
promax(loadings(fa))
}
\keyword{multivariate}
