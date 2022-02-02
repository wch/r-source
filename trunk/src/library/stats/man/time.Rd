% File src/library/stats/man/time.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{time}
\alias{time}
\alias{cycle}
\alias{frequency}
\alias{deltat}
\alias{time.default}
%\alias{time.ts}
%\alias{cycle.default}
%\alias{cycle.ts}
%\alias{frequency.default}
%\alias{deltat.default}
\title{Sampling Times of Time Series}
\description{
  \code{time} creates the vector of times at which a time series was sampled.

  \code{cycle} gives the positions in the cycle of each observation.

  \code{frequency} returns the number of samples per unit time and
  \code{deltat} the time interval between observations (see
  \code{\link{ts}}).
}
\usage{
time(x, \dots)
\method{time}{default}(x, offset = 0, \dots)

cycle(x, \dots)
frequency(x, \dots)
deltat(x, \dots)
}
\arguments{
  \item{x}{a univariate or multivariate time-series, or a vector or matrix.}
  \item{offset}{can be used to indicate when sampling took place
    in the time unit. \code{0} (the default) indicates the start
    of the unit, \code{0.5} the middle and \code{1} the end of
    the interval.}
  \item{\dots}{extra arguments for future methods.}
}
\details{
  These are all generic functions, which will use the
  \code{\link{tsp}} attribute of \code{x} if it exists. \code{time}
  and \code{cycle} have methods for class \code{\link{ts}} that coerce
  the result to that class.
}
\seealso{
  \code{\link{ts}}, \code{\link{start}}, \code{\link{tsp}},
  \code{\link{window}}.

  \code{\link{date}} for clock time, \code{\link{system.time}}
  for CPU usage.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
require(graphics)

cycle(presidents)
# a simple series plot
plot(as.vector(time(presidents)), as.vector(presidents), type = "l")
}
\keyword{ts}
