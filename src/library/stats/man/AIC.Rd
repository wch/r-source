% File src/library/stats/man/AIC.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{AIC}
\encoding{UTF-8}
\alias{AIC}
\alias{BIC}
\title{Akaike's An Information Criterion}
\description{
  Generic function calculating Akaike's \sQuote{An Information Criterion} for
  one or several fitted model objects for which a log-likelihood value
  can be obtained, according to the formula
  \eqn{-2 \mbox{log-likelihood} + k n_{par}}{-2*log-likelihood + k*npar},
  where \eqn{n_{par}}{npar} represents the number of parameters in the
  fitted model, and \eqn{k = 2} for the usual AIC, or
  \eqn{k = \log(n)}{k = log(n)}
  (\eqn{n} being the number of observations) for the so-called BIC or SBC
  (Schwarz's Bayesian criterion).
}
\usage{
AIC(object, \dots, k = 2)

BIC(object, \dots)
}
\arguments{
  \item{object}{a fitted model object for which there exists a
    \code{logLik} method to extract the corresponding log-likelihood, or
    an object inheriting from class \code{logLik}.}
  \item{\dots}{optionally more fitted model objects.}
  \item{k}{numeric, the \emph{penalty} per parameter to be used; the
    default \code{k = 2} is the classical AIC.}
}
\details{
  When comparing models fitted by maximum likelihood to the same data,
  the smaller the AIC or BIC, the better the fit.
  
  The theory of AIC requires that the log-likelihood has been maximized:
  whereas AIC can be computed for models not fitted by maximum
  likelihood, their AIC values should not be compared.

  Examples of models not \sQuote{fitted to the same data} are where the
  response is transformed (accelerated-life models are fitted to
  log-times) and where contingency tables have been used to summarize
  data.
  
  These are generic functions (with S4 generics defined in package
  \pkg{stats4}): however methods should be defined for the
  log-likelihood function \code{\link{logLik}} rather than these
  functions: the action of their default methods is to call \code{logLik}
  on all the supplied objects and assemble the results.  Note that in
  several common cases \code{\link{logLik}} does not return the value at
  the MLE: see its help page.

  The log-likelihood and hence the AIC/BIC is only defined up to an
  additive constant.  Different constants have conventionally been used
  for different purposes and so \code{\link{extractAIC}} and \code{AIC}
  may give different values (and do for models of class \code{"lm"}: see
  the help for \code{\link{extractAIC}}).  Particular care is needed
  when comparing fits of different classes (with, for example, a
  comparison of a Poisson and gamma GLM being meaningless since one has
  a discrete response, the other continuous).

  \code{BIC} is defined as
  \code{AIC(object, \dots, k = log(nobs(object)))}.
  This needs the number of observations to be known: the default method
  looks first for a \code{"nobs"} attribute on the return value from the
  \code{\link{logLik}} method, then tries the \code{\link{nobs}}
  generic, and if neither succeed returns BIC as \code{NA}.
}
\value{
  If just one object is provided, a numeric value with the corresponding
  AIC (or BIC, or \dots, depending on \code{k}).

  If multiple objects are provided, a \code{data.frame} with rows
  corresponding to the objects and columns representing the number of
  parameters in the model (\code{df}) and the AIC or BIC.
}
\references{
  Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986).
  \emph{Akaike Information Criterion Statistics}.
  D. Reidel Publishing Company.
}
\author{
  Originally by \enc{José}{Jose} Pinheiro and Douglas Bates,
  more recent revisions by R-core.
}
\seealso{
  \code{\link{extractAIC}}, \code{\link{logLik}}, \code{\link{nobs}}.
}
\examples{
lm1 <- lm(Fertility ~ . , data = swiss)
AIC(lm1)
stopifnot(all.equal(AIC(lm1),
                    AIC(logLik(lm1))))
BIC(lm1)

lm2 <- update(lm1, . ~ . -Examination)
AIC(lm1, lm2)
BIC(lm1, lm2)
}
\keyword{models}
