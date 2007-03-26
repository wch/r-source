%% From package:nlme  AIC.Rd,v 1.4 2000/07/03 18:22:45 bates
%% with additional "k = 2" argument (MM)
\name{AIC}
\alias{AIC}
%\alias{AIC.default}
%\alias{AIC.logLik}
\title{Akaike's An Information Criterion}
\description{
  Generic function calculating the Akaike information criterion for
  one or several fitted model objects for which a log-likelihood value
  can be obtained, according to the formula
  \eqn{-2 \mbox{log-likelihood} + k n_{par}}{-2*log-likelihood + k*npar},
  where \eqn{n_{par}}{npar} represents the number of parameters in the
  fitted model, and \eqn{k = 2} for the usual AIC, or \eqn{k = \log(n)}
  (\eqn{n} the number of observations) for the so-called BIC or SBC
  (Schwarz's Bayesian criterion).
}
\usage{
AIC(object, \dots, k = 2)
}
\arguments{
  \item{object}{a fitted model object, for which there exists a
    \code{logLik} method to extract the corresponding log-likelihood, or
    an object inheriting from class \code{logLik}.}
  \item{\dots}{optionally more fitted model objects.}
  \item{k}{numeric, the \dQuote{penalty} per parameter to be used; the
    default \code{k = 2} is the classical AIC.}
}
\details{
  The default method for \code{AIC}, \code{AIC.default()} entirely
  relies on the existence of a \code{\link{logLik}} method
  computing the log-likelihood for the given class.

  When comparing fitted objects, the smaller the AIC, the better the fit.

  The log-likelihood and hence the AIC is only defined up to an additive
  constant.  Different constants have conventionally be used for
  different purposes and so \code{\link{extractAIC}} and \code{AIC} may
  give different values (and do for models of class \code{"lm"}: see the
  help for \code{\link{extractAIC}}).
}
\value{
  If just one object is provided, returns a numeric value
  with the corresponding AIC (or BIC, or \dots, depending on \code{k});
  if more than one object are provided, returns a \code{data.frame} with
  rows corresponding to the objects and columns representing the number
  of parameters in the model (\code{df}) and the AIC.
}
\references{
  Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986).
  \emph{Akaike Information Criterion Statistics}.
  D. Reidel Publishing Company.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{
  \code{\link{extractAIC}}, \code{\link{logLik}}.
}
\examples{
lm1 <- lm(Fertility ~ . , data = swiss)
AIC(lm1)
stopifnot(all.equal(AIC(lm1),
                    AIC(logLik(lm1))))
## a version of BIC or Schwarz' BC :
AIC(lm1, k = log(nrow(swiss)))
}
\keyword{models}
