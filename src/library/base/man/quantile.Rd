\name{quantile}
\title{Sample Quantiles}
\usage{
quantile(x, probs=seq(0, 1, 0.25), na.rm=FALSE)
}
\alias{quantile}
\alias{quantile.default}
\description{
The generic function \code{quantile} produces sample quantiles
corresponding to the given probabilities.
The smallest observation corresponds to a probability of 0 and the
largest to a probability of 1.
}
\examples{
quantile(x <- rnorm(1000))# Extremes & Quartiles by default
quantile(x,  probs=c(.1,.5,1,2,5,10,50)/100)
}
\keyword{univar}
