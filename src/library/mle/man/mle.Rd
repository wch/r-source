\name{mle}
\alias{mle}
\title{Maximum likelihood estimation}
\description{
  Estimate parameters by the method of maximum likelihood.
}
\usage{
mle(minuslogl, start = formals(minuslogl), method = "BFGS", fixed = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minuslogl}{Function to calculate negative log-likelihood}
  \item{start}{Named list. Initial values for optimizer}
  \item{method}{Optimization method to use. See \code{optim}}
  \item{fixed}{Named list. Parameter values to keep fixed during
    optimization}
  \item{\dots}{Further arguments to pass to \code{optim}}
}
\details{
  The \code{optim} optimizer is used to find the minimum of the negative
  log-likelihood. An approximate covariance matrix for the parameters is
  obtained by inverting the Hessian matrix at the optimum.
}
\value{
  An object of class \code{"mle"}
}
\note{Be careful to note that the argument is -log L (not -2 log L). It
  is for the user to ensure that the likelihood is correct, and that
  asymptotic likelihood inference is valid.}
\seealso{\code{\link{mle-class}}}
\examples{
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
ll <- function(ymax=15,xhalf=6)
    -sum(dpois(y,lambda=ymax/(1+x/xhalf),log=TRUE))
mle(ll)
mle(ll,fixed=list(xhalf=6))
}
\keyword{models}% at least one, from doc/KEYWORDS

