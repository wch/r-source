\name{logLik}
\title{Extract Log-Likelihood}
\usage{
logLik(object, \dots)

\method{as.data.frame}{logLik}(x, row.names = NULL, optional = FALSE)
%notyet print(logLik(*), ...)
%notyet   str(logLik(*), ...)
}
\alias{logLik}
\alias{print.logLik}
\alias{str.logLik}
\alias{as.data.frame.logLik}
\arguments{
 \item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
 \item{x}{an object of class \code{logLik}.}
 \item{row.names, optional}{arguments to the \code{\link{as.data.frame}}
   method; see its documentation.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects.  Classes which already have methods for
  this function include: \code{glm}, \code{lm},
  \code{nls} in package \pkg{nls}
  and \code{gls}, \code{lme} and others in package \pkg{nlme}.
%   \code{corStruct}, \code{lmList}, \code{lmeStruct}, \code{reStruct}, and
%   \code{varFunc}.
}
\value{
  Returns an object, say \code{r}, of class \code{logLik} which is a
  number with attributes, \code{attr(r, "df")} (\bold{d}egrees of
  \bold{f}reedom) giving the number of parameters in the model.
  There's a simple \code{print} method for \code{logLik} objects.

  The details depend on the method function used; see the appropriate
  documentation.
}
\seealso{\code{\link{logLik.lm}}, \code{\link{logLik.glm}},
  \code{\link[nlme]{logLik.gls}}, \code{\link[nlme]{logLik.lme}}, etc.}
\author{Jose Pinheiro and Douglas Bates}
\examples{
## see the method function documentation
x <- 1:5
lmx <- lm(x ~ 1)
logLik(lmx) # using print.logLik() method
str(logLik(lmx))
}
\keyword{models}
