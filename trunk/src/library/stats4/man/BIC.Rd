\name{BIC}
\docType{genericFunction}
\alias{BIC}
\alias{BIC,ANY-method}
\alias{BIC,logLik-method}
\title{Bayesian Information Criterion}
\description{
  This generic function calculates the Bayesian information criterion,
  also known as Schwarz's Bayesian criterion (SBC), for one or several
  fitted model objects for which a log-likelihood value can be obtained,
  according to the formula \eqn{-2 \mbox{log-likelihood} + n_{par}
    \log(n_{obs})}{-2*log-likelihood + npar*log(nobs)}, where
  \eqn{n_{par}}{npar}  represents the
  number of parameters and \eqn{n_{obs}}{nobs} the number of
  observations in the fitted model. 
}
\usage{
BIC(object, \dots)
}
\arguments{
  \item{object}{An object of a suitable class for the BIC to be
    calculated - usually a \code{"logLik"} object or an object for which
    a \code{\link[stats4:logLik-methods]{logLik}} method exists.
  }
  \item{\dots}{Some methods for this generic function may take
    additional, optional arguments.  At present none do.}
}
\value{
  Returns a numeric value with the corresponding BIC.
}
\references{
  Schwarz, G. (1978) "Estimating the Dimension of a Model",
  \emph{Annals of Statistics}, \bold{6}, 461-464.
}
%\author{}
%\note{}
\seealso{\code{\link{logLik-methods}}, \code{\link{AIC-methods}}}
\examples{
lm1 <- lm(Fertility ~ . , data = swiss)
AIC(lm1)
BIC(lm1)

\testonly{
## 2 equivalent ways of calculating the BIC:
stopifnot(all.equal(AIC(lm1, k=log(nrow(swiss))), BIC(lm1)))
}
}
\keyword{models}

