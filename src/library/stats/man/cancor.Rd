% File src/library/stats/man/cancor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{cancor}
\title{Canonical Correlations}
\alias{cancor}
\usage{
cancor(x, y, xcenter = TRUE, ycenter = TRUE)
}
\arguments{
  \item{x}{numeric matrix (\eqn{n \times p_1}{n * p1}), containing the
    x coordinates.}
  \item{y}{numeric matrix (\eqn{n \times p_2}{n * p2}), containing the
    y coordinates.}
  \item{xcenter}{logical or numeric vector of length \eqn{p_1}{p1},
    describing any centering to be done on the x values before the
    analysis.  If \code{TRUE} (default), subtract the column means.
    If \code{FALSE}, do not adjust the columns.  Otherwise, a vector
    of values to be subtracted from the columns.}
  \item{ycenter}{analogous to \code{xcenter}, but for the y values.}
}
\description{
  Compute the canonical correlations between two data matrices.
}
\details{
  The canonical correlation analysis seeks linear combinations of the
  \code{y} variables which are well explained by linear combinations
  of the \code{x} variables. The relationship is symmetric as
  \sQuote{well explained} is measured by correlations.
}
\value{
  A list containing the following components:
  \item{cor}{correlations.}
  \item{xcoef}{estimated coefficients for the \code{x} variables.}
  \item{ycoef}{estimated coefficients for the \code{y} variables.}
  \item{xcenter}{the values used to adjust the \code{x} variables.}
  \item{ycenter}{the values used to adjust the \code{x} variables.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Hotelling H. (1936).
  Relations between two sets of variables.
  \emph{Biometrika}, \bold{28}, 321--327.
  \doi{biomet/28.3-4.321}.

  Seber, G. A. F. (1984).
  \emph{Multivariate Observations}.
  New York: Wiley.
  Page\sspace{}506f.
}
\seealso{
  \code{\link{qr}}, \code{\link{svd}}.
}
\examples{\donttest{## signs of results are random
pop <- LifeCycleSavings[, 2:3]
oec <- LifeCycleSavings[, -(2:3)]
cancor(pop, oec)

x <- matrix(rnorm(150), 50, 3)
y <- matrix(rnorm(250), 50, 5)
(cxy <- cancor(x, y))
all(abs(cor(x \%*\% cxy$xcoef,
            y \%*\% cxy$ycoef)[,1:3] - diag(cxy $ cor)) < 1e-15)
all(abs(cor(x \%*\% cxy$xcoef) - diag(3)) < 1e-15)
all(abs(cor(y \%*\% cxy$ycoef) - diag(5)) < 1e-15)
}}
\keyword{multivariate}
