% from nlme, was logLik.lm.Rd,v 1.4 2000/07/03 18:22:47 bates 
\name{logLik.lm}
\alias{logLik.lm}
\title{Extract Log-Likelihood from an lm Object}
\usage{
\method{logLik}{lm}(object, REML, \dots)
}
\arguments{
  \item{object}{an object inheriting from class \code{"lm"}.}
  \item{REML}{an optional logical value.  If \code{TRUE} the restricted
    log-likelihood is returned, else, if \code{FALSE}, the
    log-likelihood is returned.  Defaults to \code{FALSE}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\description{
  If \code{REML=FALSE}, returns the log-likelihood value of the linear
  model represented by \code{object} evaluated at the estimated
  coefficients; else, the restricted log-likelihood evaluated at the
  estimated coefficients is returned.
}
\value{
  the (restricted) log-likelihood of the linear model represented by
  \code{object} evaluated at the estimated coefficients.
}
\references{
  Harville, D.A. (1974).
  Bayesian Inference for Variance Components Using Only Error Contrasts.
  \emph{Biometrika}, \bold{61}, 383--385.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{lm}}}
\examples{
data(attitude)
(fm1 <- lm(rating ~ ., data = attitude))
logLik(fm1)
logLik(fm1, REML = TRUE)

Nnlme <- is.na(match("package:nlme", search()))
if(require(nlme)) {
  data(Orthodont)
  fm1 <- lm(distance ~ Sex * age, Orthodont)
  print(logLik(fm1))
  print(logLik(fm1, REML = TRUE))
  if(Nnlme) detach( "package:nlme")
}
}
\keyword{models}
