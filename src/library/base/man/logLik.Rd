% from nls,  logLik.Rd,v 1.1 1999/11/12 13:34:35 bates 
\name{logLik}
\title{Extract Log-Likelihood}
\usage{
logLik(object, ...)
}
\alias{logLik}
\alias{print.logLik}
\arguments{
 \item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects.  Classes which already have methods for
  this function include: \code{lm}, \code{nls} in package \bold{nls},
  and \code{gls}, \code{lme} and others in package \bold{nlme}.
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
\seealso{\code{\link{logLik.lm}}, \code{\link[nls]{logLik.nls}},
  \code{\link[nlme]{logLik.gls}}, \code{\link[nlme]{logLik.lme}}, etc.}
\author{Jose Pinheiro and Douglas Bates}
\examples{
## see the method function documentation
}
\keyword{models}
