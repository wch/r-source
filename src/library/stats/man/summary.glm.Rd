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
  \item{dispersion}{the dispersion parameter for the fitting family.
    By default it is obtained from \code{object}.}
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

  Aliased coefficients are omitted in the returned object but (as from \R
  1.8.0) restored by the \code{print} method.

  Correlations are printed to two decimal places (or symbolically): to
  see the actual correlations print \code{summary(object)$correlation}
  directly.
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
  \item{dispersion}{either the supplied argument or the estimated
    dispersion if the latter in \code{NULL}}
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
\dontshow{example("glm", echo = FALSE)}
summary(glm.D93)
}
\keyword{models}
\keyword{regression}

