% File src/library/stats/man/Uniform.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Uniform}
\alias{Uniform}
\title{The Uniform Distribution}
\usage{
dunif(x, min = 0, max = 1, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
runif(n, min = 0, max = 1)
}
\alias{dunif}
\alias{punif}
\alias{qunif}
\alias{runif}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{min, max}{lower and upper limits of the distribution.  Must be finite.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\description{
  These functions provide information about the uniform distribution
  on the interval from \code{min} to \code{max}.  \code{dunif} gives the
  density, \code{punif} gives the distribution function \code{qunif}
  gives the quantile function and \code{runif} generates random
  deviates.
}
\details{
  If \code{min} or \code{max} are not specified they assume the default
  values of \code{0} and \code{1} respectively.

  The uniform distribution has density
  \deqn{f(x) = \frac{1}{max-min}}{f(x) = 1/(max-min)}
  for \eqn{min \le x \le max}.

  For the case of \eqn{u := min == max}, the limit case of
  \eqn{X \equiv u}{X == u} is assumed, although there is no density in
  that case and \code{dunif} will return \code{NaN} (the error condition).

  \code{runif} will not generate either of the extreme values unless
  \code{max = min} or \code{max-min} is small compared to \code{min},
  and in particular not for the default arguments.
}
\value{
  \code{dunif} gives the density,
  \code{punif} gives the distribution function,
  \code{qunif} gives the quantile function, and
  \code{runif} generates random deviates.
  
  The length of the result is determined by \code{n} for
  \code{runif}, and is the maximum of the lengths of the
  numerical arguments for the other functions.  
  
  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\note{The characteristics of output from pseudo-random number generators
  (such as precision and periodicity) vary widely.  See
  \code{\link{.Random.seed}} for more information on \R's random number
  generation algorithms.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{RNG}} about random number generation in \R.

  \link{Distributions} for other standard distributions.
}
\examples{
u <- runif(20)

## The following relations always hold :
punif(u) == u
dunif(u) == 1

var(runif(10000))  #- ~ = 1/12 = .08333
}
\keyword{distribution}
