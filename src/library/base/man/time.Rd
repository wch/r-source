\name{time}
\title{Sampling Times of Time-Series}
\usage{
time(x, offset=0)
start(x)
end(x)
frequency(x)
tsp(x)
tsp(x) <- value
}
\alias{time}
\alias{start}
\alias{end}
\alias{frequency}
\alias{tsp}
\alias{tsp<-}
\alias{time.default}
\alias{start.default}
\alias{end.default}
\alias{frequency.default}
\alias{time.ts}
\alias{start.ts}
\alias{end.ts}
\alias{frequency.ts}
\arguments{
\item{x}{a time-series.}
}
\description{
  \code{time}, \code{start}, \code{end} and \code{frequency} are generic
  functions with a \code{default} and a \code{ts} method (see
  \code{\link{methods}}).

\code{time} creates the vector of times at which a
time series was sampled.
\code{offset} can be used to indicate when sampling took place
in the time unit.
\code{0} indicates the start of the unit, \code{0.5} the middle and
\code{1} the end of the interval.

\code{start} and \code{end} extract
the times the first and last observations were taken
and \code{frequency} returns the number of samples per unit time.

\code{tsp(x)} returns the vector \code{c(start(x),end(x),frequency(x))}.
It is included for compatibility with the S language.
}
\seealso{
  \code{\link{ts}}, \code{\link{window}}
  and \code{\link{system.time}} for CPU usage.
}
\examples{
data(presidents)
# a simple series plot
plot(c(time(presidents)), c(presidents), type="l")
}
\keyword{ts}
