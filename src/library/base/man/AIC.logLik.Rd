%% from package:nlme  AIC.logLik.Rd,v 1.4 2000/07/03 18:22:45 bates 
\name{AIC.logLik}
\title{AIC of a logLik Object}
\usage{
AIC(object)
}
\alias{AIC.logLik}
\arguments{
  \item{object}{an object inheriting from class \code{logLik}, usually
    resulting from applying a \code{logLik} method to a fitted model object.}
}
\description{
  see \emph{Description} in \code{\link{AIC}}.
}
\value{
  a numeric value with the corresponding AIC.
}
\references{
  Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986) "Akaike
  Information Criterion Statistics", D. Reidel Publishing Company.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{AIC}}, \code{\link{logLik}}.
\keyword{models}
