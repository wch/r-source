\name{sigma}
\title{Extract Residual Standard Deviation 'Sigma'}
\alias{sigma}
\alias{sigma.default}% lm, nls, glm
\alias{sigma.mlm}
\description{
  Extract the estimated standard deviation of the errors, the
  \dQuote{residual standard deviation} (misnomed also
  \dQuote{residual standard error}, e.g., in
  \code{\link{summary.lm}()}'s output, from a fitted model).

  Many classical statistical models have a \emph{scale parameter},
  typically the standard deviation of a zero-mean normal (or Gaussian)
  random variable which is denoted as \eqn{\sigma}.
  \code{sigma(.)} extracts the \emph{estimated} parameter from a fitted
  model, i.e., \eqn{\hat\sigma}{sigma^}.
}
\usage{
sigma(object, ...)

\S3method{sigma}{default}(object, use.fallback = TRUE, ...)
}
\arguments{
  \item{object}{an \R object, typically resulting from a model fitting
    function such as \code{\link{lm}}.}
  \item{use.fallback}{logical, passed to \code{\link{nobs}}.}
  \item{\dots}{potentially further arguments passed to and from
    methods.  Passed to \code{\link{deviance}(*, ...)} for the default method.}
}
\details{
 The \pkg{stats} package provides the S3 generic and a default method.
 The latter is correct typically for (asymptotically / approximately)
 generalized gaussian (\dQuote{least squares}) problems, since it is
 defined as \preformatted{%
   sigma.default <- function (object, use.fallback = TRUE, ...)
                      sqrt( deviance(object, ...) / (NN - PP) )
 } where \code{NN <- \link{nobs}(object, use.fallback = use.fallback)}
 and \code{PP <- length(\link{coef}(object))}.
}
\value{
  typically a number, the estimated standard deviation of the
  errors (\dQuote{residual standard deviation}) for Gaussian
  models, and---less interpretably---the square root of the residual
  deviance per degree of freedom in more general models.
  In some generalized linear modelling (\code{\link{glm}}) contexts,
  \eqn{sigma^2} (\code{sigma(.)^2}) is called \dQuote{dispersion
    (parameter)}.  Consequently, for well-fitting binomial or Poisson
  GLMs, \code{sigma} is around 1.

  Very strictly speaking, \eqn{\hat{\sigma}}{\sigma^} (\dQuote{\eqn{\sigma} hat})
  is actually \eqn{\sqrt{\widehat{\sigma^2}}}{\sqrt(hat(\sigma^2))}.

  For multivariate linear models (class \code{"mlm"}), a \emph{vector}
  of sigmas is returned, each corresponding to one column of \eqn{Y}.
}
\note{
  The misnomer \dQuote{Residual standard \bold{error}} has been part of
  too many \R (and S) outputs to be easily changed there.
}
\seealso{
  \code{\link{deviance}}, \code{\link{nobs}}, \code{\link{vcov}}.
}
\examples{
## -- lm() ------------------------------
lm1 <- lm(Fertility ~ . , data = swiss)
sigma(lm1) # ~= 7.165  = "Residual standard error"  printed from summary(lm1)
stopifnot(all.equal(sigma(lm1), summary(lm1)$sigma, tol=1e-15))

## -- nls() -----------------------------
DNase1 <- subset(DNase, Run == 1)
fm.DN1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
sigma(fm.DN1) # ~= 0.01919  as from summary(..)
stopifnot(all.equal(sigma(fm.DN1), summary(fm.DN1)$sigma, tol=1e-15))

% example from ./predict.glm.R
## -- glm() -----------------------------
## -- a) Binomial -- Example from MASS
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20-numdead)
sigma(budworm.lg <- glm(SF ~ sex*ldose, family = binomial))

## -- b) Poisson -- from ?glm :
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
sigma(glm.D93 <- glm(counts ~ outcome + treatment, family = poisson()))
## (currently) *differs* from
summary(glm.D93)$dispersion # == 1
## and the *Quasi*poisson's dispersion
sigma(glm.qD93 <- update(glm.D93, family = quasipoisson()))
sigma (glm.qD93)^2 # 1.282285 is close, but not the same
summary(glm.qD93)$dispersion # == 1.2933

## -- Multivariate lm() "mlm" -----------
utils::example("SSD", echo=FALSE)
sigma(mlmfit) # is the same as {but more efficient than}
sqrt(diag(estVar(mlmfit)))
\dontshow{stopifnot(all.equal(sigma(mlmfit), sqrt(diag(estVar(mlmfit)))))}
}
\keyword{models}
