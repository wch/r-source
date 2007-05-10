\name{summary.glm}
\alias{summary.glm}
%\alias{summary.glm.null}
\alias{print.summary.glm}
%\alias{print.summary.glm.null}
\title{Summarizing Generalized Linear Model Fits}
\usage{
\method{summary}{glm}(object, dispersion = NULL, correlation = FALSE,
        symbolic.cor = FALSE, \dots)

\method{print}{summary.glm}(x, digits = max(3, getOption("digits") - 3),
      symbolic.cor = x$symbolic.cor,
      signif.stars = getOption("show.signif.stars"), \dots)
}
\arguments{
  \item{object}{an object of class \code{"glm"}, usually, a result of a
    call to \code{\link{glm}}.}
  \item{x}{an object of class \code{"summary.glm"}, usually, a result of a
    call to \code{summary.glm}.}
  \item{dispersion}{the dispersion parameter for the family used.
    Either a single numerical value or \code{NULL} (the default), when
    it is inferred from \code{object} (see Details).}
  \item{correlation}{logical; if \code{TRUE}, the correlation matrix of
    the estimated parameters is returned and printed.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{symbolic.cor}{logical. If \code{TRUE}, print the correlations in
    a symbolic form (see \code{\link{symnum}}) rather than as numbers.}
  \item{signif.stars}{logical. If \code{TRUE}, \dQuote{significance stars}
    are printed for each coefficient.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  These functions are all \code{\link{methods}} for class \code{glm} or
  \code{summary.glm} objects.
}
\details{
  \code{print.summary.glm} tries to be smart about formatting the
  coefficients, standard errors, etc. and additionally gives
  \dQuote{significance stars} if \code{signif.stars} is \code{TRUE}.
  The \code{coefficients} component of the result gives the estimated
  coefficients and their estimated standard errors, together with their
  ratio.  This third column is labelled \code{t ratio} if the
  dispersion is estimated, and \code{z ratio} if the dispersion is known
  (or fixed by the family).  A fourth column gives the two-tailed
  p-value corresponding to the t or z ratio based on a Student t or
  Normal reference distribution.  (It is possible that the dispersion is
  not known and there are no residual degrees of freedom from which to
  estimate it.  In that case the estimate is \code{NaN}.)

  Aliased coefficients are omitted in the returned object but restored
  by the \code{print} method.

  Correlations are printed to two decimal places (or symbolically): to
  see the actual correlations print \code{summary(object)$correlation}
  directly.

  The dispersion of a GLM is not used in the fitting process, but it is
  needed to find standard errors.
  If \code{dispersion} is not supplied or \code{NULL},
  the dispersion is taken as \code{1} for the \code{binomial} and
  \code{Poisson} families, and otherwise estimated by the residual
  Chisquared statistic (calculated from cases with non-zero weights)
  divided by the residual degrees of freedom.

  \code{summary} can be used with Gaussian \code{glm} fits to handle the
  case of a linear regression with known error variance, something not
  handled by \code{\link{summary.lm}}.
}
\value{
  \code{summary.glm} returns an object of class \code{"summary.glm"}, a
  list with components

  \item{call}{the component from \code{object}.}
  \item{family}{the component from \code{object}.}
  \item{deviance}{the component from \code{object}.}
  \item{contrasts}{the component from \code{object}.}
  \item{df.residual}{the component from \code{object}.}
  \item{null.deviance}{the component from \code{object}.}
  \item{df.null}{the component from \code{object}.}
  \item{deviance.resid}{the deviance residuals:
    see \code{\link{residuals.glm}}.}
  \item{coefficients}{the matrix of coefficients, standard errors,
    z-values and p-values.  Aliased coefficients are omitted.}
  \item{aliased}{named logical vector showing if the original
    coefficients are aliased.} 
  \item{dispersion}{either the supplied argument or the inferred/estimated
    dispersion if the latter is \code{NULL}.}
  \item{df}{a 3-vector of the rank of the model and the number of
    residual degrees of freedom, plus number of non-aliased coefficients.}
  \item{cov.unscaled}{the unscaled (\code{dispersion = 1}) estimated covariance
    matrix of the estimated coefficients.}
  \item{cov.scaled}{ditto, scaled by \code{dispersion}.}
  \item{correlation}{(only if \code{correlation} is true.)  The estimated
    correlations of the estimated coefficients.}
  \item{symbolic.cor}{(only if \code{correlation} is true.)  The value
    of the argument \code{symbolic.cor}.}
}
\seealso{
  \code{\link{glm}}, \code{\link{summary}}.
}
\examples{
## --- Continuing the Example from  '?glm':%\code{\link{glm}}:
\dontshow{utils::example("glm", echo = FALSE)}
summary(glm.D93)
}
\keyword{models}
\keyword{regression}

