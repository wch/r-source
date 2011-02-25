% File src/library/stats/man/AIC.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{AIC}
\encoding{UTF-8}
\alias{AIC}
\alias{BIC}
\title{Akaike's An Information Criterion}
\description{
  Generic function calculating the Akaike information criterion for
  one or several fitted model objects for which a log-likelihood value
  can be obtained, according to the formula
  \eqn{-2 \mbox{log-likelihood} + k n_{par}}{-2*log-likelihood + k*npar},
  where \eqn{n_{par}}{npar} represents the number of parameters in the
  fitted model, and \eqn{k = 2} for the usual AIC, or
  \eqn{k = \log(n)}{k = log(n)}
  (\eqn{n} the number of observations) for the so-called BIC or SBC
  (Schwarz's Bayesian criterion).
}
\usage{
AIC(object, \dots, k = 2)

BIC(object, \dots)
}
\arguments{
  \item{object}{a fitted model object, for which there exists a
    \code{logLik} method to extract the corresponding log-likelihood, or
    an object inheriting from class \code{logLik}.}
  \item{\dots}{optionally more fitted model objects.}
  \item{k}{numeric, the \emph{penalty} per parameter to be used; the
    default \code{k = 2} is the classical AIC.}
}
\details{
  The default method for \code{AIC}, \code{AIC.default()} entirely
  relies on the existence of a \code{\link{logLik}} method
  computing the log-likelihood for the given class.

  When comparing fitted objects, the smaller the AIC (or BIC), the
  better the fit.

  The log-likelihood and hence the AIC is only defined up to an additive
  constant.  Different constants have conventionally be used for
  different purposes and so \code{\link{extractAIC}} and \code{AIC} may
  give different values (and do for models of class \code{"lm"}: see the
  help for \code{\link{extractAIC}}).
  
  \code{BIC} is defined as \code{AIC(object, \dots, k = log(nobs(object)))}.
  
  For historical reasons these are generic functions, but methods should
  be defined for \code{\link{logLik}} and \code{\link{nobs}} rather than
  for these functions.
}
\value{
  If just one object is provided, returns a numeric value
  with the corresponding AIC (or BIC, or \dots, depending on \code{k});
  if multiple objects are provided, returns a \code{data.frame} with
  rows corresponding to the objects and columns representing the number
  of parameters in the model (\code{df}) and the AIC or BIC.
}
\note{
  Prior to \R 2.13.0, an S3 generic for \code{BIC} was defined in
  package \pkg{nlme}, and there have long been S4 generics for
  \code{AIC} and \code{BIC} in package \pkg{stats4}.
}
\references{
  Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986).
  \emph{Akaike Information Criterion Statistics}.
  D. Reidel Publishing Company.
}
\author{
  \enc{José}{Jose} Pinheiro and Douglas Bates, R-core.
}
\seealso{
  \code{\link{extractAIC}}, \code{\link{logLik}}, \code{\link{nobs}}.
}
\examples{
lm1 <- lm(Fertility ~ . , data = swiss)
AIC(lm1)
stopifnot(all.equal(AIC(lm1), AIC(logLik(lm1))))

BIC(lm1)
## This can also be computed as
BIC(logLik(lm1))
AIC(lm1, k = log(nobs(lm1)))

lm2 <- update(lm1, . ~ . -Examination)
AIC(lm1, lm2)
BIC(lm1, lm2)
}
\keyword{models}
