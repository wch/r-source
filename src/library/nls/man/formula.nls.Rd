% $Id: formula.nls.Rd,v 1.3 2001/03/24 06:57:57 ripley Exp $
\name{formula.nls}
\title{Extract Model Formula from nls Object}
\usage{
formula(object)
}
\alias{formula.nls}
\arguments{
 \item{object}{an object inheriting from class \code{nls}, representing
   a nonlinear least squares fit.}
}
\description{
  Returns the model used to fit \code{object}. 
}
\value{
  a formula representing the model used to obtain \code{object}.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{formula}}}
\examples{
data(Orange)
fm1 <- nls(circumference ~ A/(1+exp((B-age)/C)), Orange,
  start = list(A=160, B=700, C = 350))
formula(fm1)
}
\keyword{models}
