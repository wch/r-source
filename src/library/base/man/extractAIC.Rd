%-- This page by Martin Maechler,  improvements welcome!
\name{extractAIC}
\title{Extract AIC from a Fitted Model}
%
\alias{extractAIC.aov}  
\alias{extractAIC.lm}
\alias{extractAIC.glm}    
\alias{extractAIC.coxph}  
\alias{extractAIC.negbin}
\alias{extractAIC.survreg}
\usage{
extractAIC    (fit, scale,     k = 2, \dots)  
extractAIC.lm (fit, scale = 0, k = 2, \dots)
extractAIC.glm(fit, scale = 0, k = 2, \dots)  
extractAIC.aov(fit, scale = 0, k = 2, \dots)  
extractAIC.coxph  (fit, scale, k = 2, \dots)  
extractAIC.negbin (fit, scale, k = 2, \dots)  
extractAIC.survreg(fit, scale, k = 2, \dots)  
}
\arguments{
  \item{fit}{fitted model, usually the result of a fitter like
    \code{\link{lm}}.}
 \item{scale}{optional numeric specifying the scale parameter of the
   model, see \code{scale} in \code{\link{step}}.
 }
 \item{k}{numeric specifying the ``weight'' of the
   \emph{equivalent degrees of freedom} (\eqn{\equiv}{=:}\code{edf})
   part in the AIC formula.}
 \item{\dots}{further arguments (currently unused in base \R).}
}
%-- Source in ../R/add.R
\description{
  Computes the (generalized) \bold{A}kaike \bold{I}nformation
  \bold{C}riterion for a fitted parametric model.
}
\details{
  The criterion used is 
 \deqn{AIC = - 2\log L +  k \times \mbox{edf},}{AIC = - 2*log L +  k * edf,}
  where \eqn{L} is the likelihood
  and \code{edf} the equivalent degrees of freedom (i.e., the number of
  parameters for usual parametric models) of \code{fit}.
 
  For generalized linear models (i.e., for \code{\link{lm}},
  \code{\link{aov}}, and \code{\link{glm}}), \eqn{-2\log L} is
  the \emph{deviance}, as computed by \code{\link{deviance}(fit)}.
    
  \code{k = 2} corresponds to the traditional AIC, using \code{k =
    log(n)} provides the BIC (Bayes IC) instead.
  
  For further information, particularly about \code{scale}, see
  \code{\link{step}}.
}
\note{These functions are used in \code{\link{add1}},
  \code{\link{drop1}} and \code{\link{step}} and that may be their
  main use.
}
\value{
  A numeric vector of length 2, giving
  \item{edf}{the ``\bold{e}quivalent \bold{d}egrees of \bold{f}reedom''
  of the fitted model \code{fit}.}
  \item{AIC}{the (generalized) Akaike Information Criterion for \code{fit}.}
}
%-- Source in ../R/add.R
\author{B. D. Ripley}
\references{
  Venables, W. N. and Ripley, B. D. (1997)
  \emph{Modern Applied Statistics with S-PLUS.}
  New York: Springer (2nd ed).
}
\seealso{\code{\link{deviance}}, \code{\link{add1}}, \code{\link{step}}}
\examples{
example(glm)
extractAIC(glm.D93)#>>  5  15.129
}
\keyword{models}

