\name{quantile}
\title{Sample Quantiles}
\alias{quantile}
\alias{quantile.default}
\description{
  The generic function \code{quantile} produces sample quantiles
  corresponding to the given probabilities.
  The smallest observation corresponds to a probability of 0 and the
  largest to a probability of 1.
}
\usage{
quantile(x, \dots)

\method{quantile}{default}(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, \dots)
}
\arguments{
  \item{x}{numeric vectors whose sample quantiles are wanted.}
  \item{probs}{numeric vector with values in \eqn{[0,1]}.}
  \item{na.rm}{logical; if true, any \code{\link{NA}} and \code{NaN}'s
    are removed from \code{x} before the quantiles are computed.}
  \item{names}{logical; if true, the result has a \code{\link{names}}
    attribute.  Set to \code{FALSE} for speedup with many \code{probs}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  A vector of length \code{length(probs)} is returned;
  if \code{names = TRUE}, it has a \code{\link{names}} attribute.

  \code{quantile(x,p)} as a function of \code{p} linearly interpolates
  the points \eqn{( (i-1)/(n-1), ox[i] )}, where
  \code{ox <- sort(x)} and \code{n <- length(x)}.

  This gives \code{quantile(x, p) == (1-f)*ox[i] + f*ox[i+1]}, where
  \code{r <- 1 + (n-1)*p}, \code{i <- floor(r)}, \code{f <- r - i}
  \emph{and} \code{ox[n+1] :=  ox[n]}.

  \code{\link{NA}} and \code{\link{NaN}} values in \code{probs} are
  propagated to the result.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{ecdf}} (in the \pkg{stats} package) for
  empirical distributions of which \code{quantile} is the
  \dQuote{inverse};
  \code{\link{boxplot.stats}} and \code{\link{fivenum}} for computing
  \dQuote{versions} of quartiles, etc.
}
\examples{
quantile(x <- rnorm(1001))# Extremes & Quartiles by default
quantile(x,  probs=c(.1,.5,1,2,5,10,50, NA)/100)
}
\keyword{univar}
