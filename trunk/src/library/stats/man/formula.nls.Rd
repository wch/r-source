% $Id: formula.nls.Rd,v 1.2 2003/12/11 17:27:18 ripley Exp $
\name{formula.nls}
\alias{formula.nls}
\title{Extract Model Formula from nls Object}
\usage{
\method{formula}{nls}(x, \dots)
}
\arguments{
 \item{x}{an object inheriting from class \code{"nls"}, representing
   a nonlinear least squares fit.}
 \item{\dots}{further arguments passed to or from other methods.}
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
fm1 <- nls(circumference ~ A/(1+exp((B-age)/C)), Orange,
           start = list(A=160, B=700, C = 350))
formula(fm1)
}
\keyword{models}
