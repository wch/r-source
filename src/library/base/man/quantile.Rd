\name{quantile}
\title{Sample Quantiles}
\usage{
quantile(x, probs=seq(0, 1, 0.25), na.rm=FALSE, names = TRUE)
}
\alias{quantile}
\alias{quantile.default}
\description{
The generic function \code{quantile} produces sample quantiles
corresponding to the given probabilities.
The smallest observation corresponds to a probability of 0 and the
largest to a probability of 1.
}
\details{
A vector of length \code{length(probs)} is returned;
if \code{names = TRUE}, it has a \code{\link{names}} attribute.

\code{quantile(x,p)} as a function of \code{p} linearly interpolates
the points ( (i-1)/(n-1), ox[i] ), where
\code{ox <- order(x)} (the ``order statistics'') and \code{n <- length(x)}.

This gives \code{quantile(x, p) == (1-f)*ox[i] + f*ox[i+1]}, where
\code{r <- 1 + (n-1)*p}, \code{i <- floor(r)}, \code{f <- r - i}
\emph{and} \code{ox[n+1] :=  ox[n]}.
}
\examples{
quantile(x <- rnorm(1001))# Extremes & Quartiles by default
quantile(x,  probs=c(.1,.5,1,2,5,10,50)/100)

n <- length(x) ## the following is exact, because 1/(1001-1) is exact:
all(abs(sort(x) == quantile(x, probs = ((1:n)-1)/(n-1), names=F)))# TRUE

n <- 777
ox <- sort(x <- round(rnorm(n),1))# round() produces ties
ox <- c(ox, ox[n]) #- such that ox[n+1] := ox[n]
p <- c(0,1,runif(100))
i <- floor(r <- 1 + (n-1)*p)
f <- r - i
all(abs(quantile(x,p) - ((1-f)*ox[i] + f*ox[i+1])) < 20*.Machine$double.eps)
}
\keyword{univar}
