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
\seealso{\code{\link{glm}}, \code{\link{logLik.lm}}}
\examples{
\testonly{example("glm", echo = FALSE)}
##-- Continuing the  glm(.) example:
Eq <- function(x,y) all.equal(x,y, tol = 1e-12)
stopifnot(Eq(AIC(anorex.1), anorex.1$aic),
          Eq(AIC(g1), g1$aic),
          Eq(AIC(g2), g2$aic))
}
\keyword{models}
