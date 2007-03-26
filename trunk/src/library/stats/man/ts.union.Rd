\name{ts.union}
\alias{ts.union}
\alias{ts.intersect}
\title{Bind Two or More Time Series}
\usage{
ts.intersect(\dots, dframe = FALSE)
ts.union(\dots, dframe = FALSE)
}
\arguments{
  \item{\dots}{two or more univariate or multivariate time series, or
    objects which can coerced to time series.}
  \item{dframe}{logical; if \code{TRUE} return the result as a data
    frame.}
}
\description{
  Bind time series which have a common frequency. \code{ts.union} pads
  with \code{NA}s to the total time coverage, \code{ts.intersect}
  restricts to the time covered by all the series.
}
\details{
  As a special case, \code{\dots} can contain vectors or matrices of the
  same length as the combined time series of the time series present, as
  well as those of a single row.
}
\value{
  A time series object if \code{dframe} is \code{FALSE}, otherwise a
  data frame.
}
\seealso{\code{\link{cbind}}.}
\examples{
ts.union(mdeaths, fdeaths)
cbind(mdeaths, fdeaths) # same as the previous line
ts.intersect(window(mdeaths, 1976), window(fdeaths, 1974, 1978))

sales1 <- ts.union(BJsales, lead = BJsales.lead)
ts.intersect(sales1, lead3 = lag(BJsales.lead, -3))
}
\keyword{ts}
