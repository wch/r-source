\name{ts.union}
\alias{ts.union}
\alias{ts.intersect}
\alias{cbind.ts}
\title{Bind Two or More Time Series}
\usage{
ts.intersect(..., dframe=FALSE)
ts.union(..., dframe=FALSE)
cbind.ts(..., dframe=FALSE, union=TRUE)
}
\arguments{
 \item{\dots}{Two or more univariate or multivariate time series, or
     objects which can coerced to time series.}
 \item{dframe}{If \code{TRUE} return the result as a data frame.}
 \item{union}{Act as \code{ts.union} or \code{ts.intersect}.}
}
\description{
    Bind time series which have a common frequency. \code{ts.union} pads with
    NAs to the total time coverage, \code{ts.intersect} restricts to the
    time covered by all the series.

    As a special case, \code{\dots} can contain vectors or matrices of
    the same length as the combined time series of the time series
    present, as well as those of a single row.
}
\value{
    A time series object if \code{dframe} is \code{FALSE}, otherwise
    a data frame.
}
\author{B.D. Ripley}
\examples{
data(UKLungDeaths)
ts.union(mdeaths, fdeaths)
cbind(mdeaths, fdeaths) # same as the previous line
ts.intersect(window(mdeaths, 1976), window(fdeaths, 1974, 1978))
data(BJsales)
sales1 <- ts.union(BJsales, lead=BJsales.lead)
ts.intersect(sales1, lead3=lag(BJsales.lead, -3))
}
\keyword{ts}
