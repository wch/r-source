\name{cycle}
\alias{cycle}
\alias{deltat}
\title{Sampling Times of Time-Series}
\usage{
cycle(x, ...)
deltat(x, ...)
}
\arguments{
 \item{x}{A univariate or multivariate time series.}
 \item{\dots}{Arguments for methods: currently unused.}
}
\description{
    \code{cycle} gives the positions in the cycle of each observation.
    \code{delta} gives the time interval between observations.

    Both are generic functions.
}
\author{B.D. Ripley}

\seealso{\code{\link{time}}, \code{\link{frequency}} }

\examples{
data(presidents)
deltat(presidents)
cycle(presidents)
}

\keyword{ts}
