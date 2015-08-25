% File src/library/stats/man/summary.nls.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{summary.nls}
\alias{summary.nls}
\alias{print.summary.nls}
\title{Summarizing Non-Linear Least-Squares Model Fits}
\description{
  \code{summary} method for class \code{"nls"}.
}
\usage{
\method{summary}{nls}(object, correlation = FALSE, symbolic.cor = FALSE, \dots)

\method{print}{summary.nls}(x, digits = max(3, getOption("digits") - 3),
      symbolic.cor = x$symbolic.cor,
      signif.stars = getOption("show.signif.stars"), \dots)
}
\arguments{
  \item{object}{an object of class \code{"nls"}.}
  \item{x}{an object of class \code{"summary.nls"}, usually the result of a
    call to \code{summary.nls}.}
  \item{correlation}{logical; if \code{TRUE}, the correlation matrix of
    the estimated parameters is returned and printed.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{symbolic.cor}{logical.  If \code{TRUE}, print the correlations in
    a symbolic form (see \code{\link{symnum}}) rather than as numbers.}
  \item{signif.stars}{logical.  If \code{TRUE}, \sQuote{significance stars}
    are printed for each coefficient.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The distribution theory used to find the distribution of the
  standard errors and of the residual standard error (for t ratios) is
  based on linearization and is approximate, maybe very approximate.

  \code{print.summary.nls} tries to be smart about formatting the
  coefficients, standard errors, etc. and additionally gives
  \sQuote{significance stars} if \code{signif.stars} is \code{TRUE}.

  Correlations are printed to two decimal places (or symbolically): to
  see the actual correlations print \code{summary(object)$correlation}
  directly.
}
\value{
  The function \code{summary.nls} computes and returns a list of summary
  statistics of the fitted model given in \code{object}, using
  the component  \code{"formula"} from its argument, plus
  \item{residuals}{the \emph{weighted} residuals, the usual residuals
    rescaled by the square root of the weights specified in the call to
    \code{nls}.}
  \item{coefficients}{a \eqn{p \times 4}{p x 4} matrix with columns for
    the estimated coefficient, its standard error, t-statistic and
    corresponding (two-sided) p-value.}
  \item{sigma}{the square root of the estimated variance of the random
    error
    \deqn{\hat\sigma^2 = \frac{1}{n-p}\sum_i{R_i^2},}{\sigma^2 = 1/(n-p) Sum(R[i]^2),}
    where \eqn{R_i}{R[i]} is the \eqn{i}-th weighted residual.}
  \item{df}{degrees of freedom, a 2-vector \eqn{(p, n-p)}.  (Here and
    elsewhere \eqn{n} omits observations with zero weights.)}
  \item{cov.unscaled}{a \eqn{p \times p}{p x p} matrix of (unscaled)
    covariances of the parameter estimates.}
  \item{correlation}{the correlation matrix corresponding to the above
    \code{cov.unscaled}, if \code{correlation = TRUE} is specified and
    there are a non-zero number of residual degrees of freedom.}
  \item{symbolic.cor}{(only if \code{correlation} is true.)  The value
    of the argument \code{symbolic.cor}.}
}
\seealso{
  The model fitting function \code{\link{nls}}, \code{\link{summary}}.

  Function \code{\link{coef}} will extract the matrix of coefficients
  with standard errors, t-statistics and p-values.
}

\keyword{regression}
\keyword{models}
