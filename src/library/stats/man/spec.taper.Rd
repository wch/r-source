\name{spec.taper}
\alias{spec.taper}
\title{Taper a Time Series by a Cosine Bell}
\usage{
spec.taper(x, p = 0.1)
}
\arguments{
 \item{x}{A univariate or multivariate time series}
 \item{p}{The total proportion to be tapered, either a scalar or a
   vector of the length of the number of series.}
}
\description{
  Apply a cosine-bell taper to a time series.
}
\details{
  The cosine-bell taper is applied to the first and last \code{p[i]/2}
  observations of time series \code{x[, i]}.
}
\value{
  A new time series object.
}
\author{Kurt Hornik, B.D. Ripley}
\note{From package \pkg{MASS}.}

\seealso{
  \code{\link{spec.pgram}},
  \code{\link{cpgram}}
}

\keyword{ts}
