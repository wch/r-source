% File src/library/stats/man/logLik.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{logLik}
\encoding{UTF-8}
\alias{logLik}
%\alias{print.logLik}
%\alias{str.logLik}
\alias{logLik.lm}
\title{Extract Log-Likelihood}
\usage{
logLik(object, \dots)

\method{logLik}{lm}(object, REML = FALSE, \dots)
}
\arguments{
 \item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
  \item{REML}{an optional logical value.  If \code{TRUE} the restricted
    log-likelihood is returned, else, if \code{FALSE}, the
    log-likelihood is returned.  Defaults to \code{FALSE}.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects.  Classes which have methods for this
  function include: \code{"glm"}, \code{"lm"}, \code{"nls"} and
  \code{"Arima"}.  Packages contain methods for other classes, such as
  \code{"fitdistr"}, \code{"negbin"} and \code{"polr"} in package
  \CRANpkg{MASS}, \code{"multinom"} in package \CRANpkg{nnet} and
  \code{"gls"}, \code{"gnls"} \code{"lme"} and others in package
  \CRANpkg{nlme}.
}
%   \code{corStruct}, \code{lmList}, \code{lmeStruct}, \code{reStruct}, and
%   \code{varFunc}.
\details{
  \code{logLik} is most commonly used for a model fitted by maximum
  likelihood, and some uses, e.g.\sspace{}by \code{\link{AIC}}, assume
  this.  So care is needed where other fit criteria have been used, for
  example REML (the default for \code{"lme"}).

  For a \code{"glm"} fit the \code{\link{family}} does not have to
  specify how to calculate the log-likelihood, so this is based on using
  the family's \code{aic()} function to compute the AIC.  For the
  \code{\link{gaussian}}, \code{\link{Gamma}} and
  \code{\link{inverse.gaussian}} families it assumed that the dispersion
  of the GLM is estimated and has been counted as a parameter in the AIC
  value, and for all other families it is assumed that the dispersion is
  known.  Note that this procedure does not give the maximized
  likelihood for \code{"glm"} fits from the Gamma and inverse gaussian
  families, as the estimate of dispersion used is not the MLE.

  For \code{"lm"} fits it is assumed that the scale has been estimated
  (by maximum likelihood or REML), and all the constants in the
  log-likelihood are included.  That method is only applicable to
  single-response fits.
}
\value{
  Returns an object of class \code{logLik}.  This is a number with at
  least one attribute, \code{"df"} (\bold{d}egrees of \bold{f}reedom),
  giving the number of (estimated) parameters in the model.

  There is a simple \code{print} method for \code{"logLik"} objects.

  There may be other attributes depending on the method used: see the
  appropriate documentation.  One that is used by several methods is
  \code{"nobs"}, the number of observations used in estimation (after
  the restrictions if \code{REML = TRUE}).
}
\seealso{
  \code{\link[nlme:logLik.lme]{logLik.gls}}, \code{\link[nlme]{logLik.lme}}, in
  package \CRANpkg{nlme}, etc.

  \code{\link{AIC}}
}
\references{
  For \code{logLik.lm}:

  Harville, D.A. (1974).
  Bayesian inference for variance components using only error contrasts.
  \emph{Biometrika}, \bold{61}, 383--385.
}
\author{
  \enc{José}{Jose} Pinheiro and Douglas Bates
}
\examples{
x <- 1:5
lmx <- lm(x ~ 1)
logLik(lmx) # using print.logLik() method
utils::str(logLik(lmx))

## lm method
(fm1 <- lm(rating ~ ., data = attitude))
logLik(fm1)
logLik(fm1, REML = TRUE)

\donttest{utils::data(Orthodont, package = "nlme")
fm1 <- lm(distance ~ Sex * age, Orthodont)
logLik(fm1)
logLik(fm1, REML = TRUE)
}}
\keyword{models}
