\name{logLik}
\alias{logLik}
\alias{print.logLik}
\alias{str.logLik}
% \alias{as.data.frame.logLik}
\alias{logLik.lm}
\title{Extract Log-Likelihood}
\usage{
logLik(object, \dots)

\method{logLik}{lm}(object, REML = FALSE, \dots)
%
%\method{as.data.frame}{logLik}(x, row.names = NULL, optional = FALSE)
%notyet print(logLik(*), ...)
%notyet   str(logLik(*), ...)
}
\arguments{
 \item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
  \item{REML}{an optional logical value.  If \code{TRUE} the restricted
    log-likelihood is returned, else, if \code{FALSE}, the
    log-likelihood is returned.  Defaults to \code{FALSE}.}
% \item{x}{an object of class \code{logLik}.}
% \item{row.names, optional}{arguments to the \code{\link{as.data.frame}}
%   method; see its documentation.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects.  Classes which already have methods for
  this function include: \code{glm}, \code{lm}, \code{nls}
  and \code{gls}, \code{lme} and others in package \pkg{nlme}.
%   \code{corStruct}, \code{lmList}, \code{lmeStruct}, \code{reStruct}, and
%   \code{varFunc}.
}
\details{
  For a \code{glm} fit the \code{\link{family}} does not have to specify
  how to calculate the log-likelihood, so this is based on the
  family's function to compute
  the AIC.  For \code{\link{gaussian}}, \code{\link{Gamma}} and
  \code{\link{inverse.gaussian}}
  families it assumed that the dispersion of the GLM is estimated and
  has been included in the AIC, and for all other families it is assumed
  that the dispersion is known.

  Note that this procedure is not completely accurate for the gamma and
  inverse gaussian families, as the estimate of dispersion used is not
  the MLE.
}
\value{
  Returns an object, say \code{r}, of class \code{logLik} which is a
  number with attributes, \code{attr(r, "df")} (\bold{d}egrees of
  \bold{f}reedom) giving the number of parameters in the model.
  There's a simple \code{print} method for \code{logLik} objects.

  The details depend on the method function used; see the appropriate
  documentation.
}
\seealso{
  \code{\link[nlme]{logLik.gls}}, \code{\link[nlme]{logLik.lme}}, in
  package \pkg{nlme}, etc.
}
\references{
  For \code{logLik.lm}:
  
  Harville, D.A. (1974).
  Bayesian inference for variance components using only error contrasts.
  \emph{Biometrika}, \bold{61}, 383--385.
}
\author{Jose Pinheiro and Douglas Bates}
\examples{
x <- 1:5
lmx <- lm(x ~ 1)
logLik(lmx) # using print.logLik() method
str(logLik(lmx))

## lm method
(fm1 <- lm(rating ~ ., data = attitude))
logLik(fm1)
logLik(fm1, REML = TRUE)

res <- try(data(Orthodont, package="nlme"))
if(!inherits(res, "try-error")) {
  fm1 <- lm(distance ~ Sex * age, Orthodont)
  print(logLik(fm1))
  print(logLik(fm1, REML = TRUE))
}
}
\keyword{models}
