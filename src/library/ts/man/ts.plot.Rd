\name{ts.plot}
\alias{ts.plot}
\title{Plot Multiple Time Series}
\usage{
ts.plot(..., gpars=list())
}
\arguments{
  \item{\dots}{one or more univariate or multivariate time series}
  \item{gpars}{list of named graphics parameters to be passed to the
    plotting functions}
}
\description{
  Plot several time series on a common plot. Unlike
  \code{\link{plot.ts}} the series can have a different time bases,
  but they should have the same frequency.
}
\note{
  Although this can be used for a single time series, \code{plot} is
  easier to use and is preferred.
}
\value{
  None.
}
\author{B.D. Ripley}


\seealso{\code{\link{plot.ts}}}

\examples{
data(UKLungDeaths)
ts.plot(ldeaths, mdeaths, fdeaths,
        gpars=list(xlab="year", ylab="deaths", lty=c(1:3)))

data(nottem)
nott <- window(nottem, end=c(1936,12))
fit <- arima0(nott,order=c(1,0,0), list(order=c(2,1,0), period=12))
nott.fore <- predict(fit, n.ahead=36)
ts.plot(nott, nott.fore$pred, nott.fore$pred+2*nott.fore$se,
        nott.fore$pred-2*nott.fore$se, gpars=list(col=c(1,1,4,4)))
}
\keyword{ts}
