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
 \item{\dots}{Two or more univariate or multvariate time series.}
 \item{dframe}{If \code{TRUE} return the result as a data frame.}
 \item{union}{Act as \code{ts.union} or \code{ts.intersect}.}
}
\description{
    Bind time series which have a common frequency. \code{ts.union} pads with
    NAs to the total time coverage, \code{ts.intersect} restricts to the
    time covered by all the series.
}
\value{
    A time series object if \code{dframe} is \code{FALSE}, otherwise
    a data frame.
}
\author{B.D. Ripley}
\examples{
data(deaths)
ts.union(mdeaths, fdeaths)
ts.intersect(window(mdeaths, 1976), window(fdeaths, 1974, 1978))
data(sales)
sales1 <- ts.union(sales, lead=sales.lead)
ts.intersect(sales1, lead3=lag(sales.lead, -3))
}
\keyword{ts}