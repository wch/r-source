% $Id: logLik.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
\name{logLik}
\title{Extract Log-Likelihood}
\usage{
logLik(object, ...)
}
\alias{logLik}
\arguments{
 \item{object}{any object from which a log-likelihood value, or a
   contribution to a log-likelihood value, can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include: \code{corStruct}, \code{gls}, \code{lm},
  \code{lme}, \code{lmList}, \code{lmeStruct}, \code{reStruct}, and
  \code{varFunc}. 
}
\value{
  will depend on the method function used; see the appropriate
  documentation.  
}
\author{Jose Pinheiro and Douglas Bates}
\examples{
## see the method function documentation
}
\keyword{models}
