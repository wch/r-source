\name{logLik.glm}
\alias{logLik.glm}
\title{Extract Log-Likelihood from an glm Object}
\usage{
\method{logLik}{glm}(object, \dots)
}
\arguments{
  \item{object}{an object inheriting from class \code{"glm"}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\description{
  Returns the log-likelihood value of the generalized linear
  model represented by \code{object} evaluated at the estimated
  coefficients.
}
\value{
  the log-likelihood of the linear model represented by
  \code{object} evaluated at the estimated coefficients.
}
\details{
  As a \code{\link{family}} does not have to specify how to calculate
  the log-likelihood, this is based on the family's function to compute
  the AIC.  For \code{\link{gaussian}}, \code{\link{Gamma}} and
  \code{\link{inverse.gaussian}}
  families it assumed that the dispersion of the GLM is estimated and
  has been included in the AIC, and for all other families it is assumed
  that the dispersion is known.

  Not that this procedure is not completely accurate for the gamma and
  inverse gaussian families, as the estimate of dispersion used is not
  the MLE.
}
\seealso{\code{\link{glm}}, \code{\link{logLik.lm}}}
\keyword{models}
