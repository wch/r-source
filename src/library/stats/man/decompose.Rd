\name{decompose}
\alias{decompose}
\alias{plot.decomposed.ts}
\title{
  Classical Seasonal Decomposition by Moving Averages
}
\description{
  Decompose a time series into seasonal, trend and irregular components
  using moving averages.  Deals with additive or multiplicative
  seasonal component.
}
\usage{
decompose(x, type = c("additive", "multiplicative"), filter = NULL)
}
\arguments{
  \item{x}{A time series.}
  \item{type}{The type of seasonal component.}
  \item{filter}{A vector of filter coefficients in reverse time order (as for
    AR or MA coefficients), used for filtering out the seasonal
    component. If \code{NULL}, a moving average with symmetric window is
  performed.}
}
\details{
The additive model used is:

Y[t] = T[t] + S[t] + e[t]

The multiplicative model used is:

Y[t] = T[t] * S[t] + e[t]
}
\note{
  The function \code{\link{stl}} provides a much more sophisticated
  decomposition.
}
\value{
  An object of class \code{"decomposed.ts"} with following components:
  \item{seasonal}{The seasonal component (i.e., the repeated seasonal figure)}
  \item{figure}{The estimated seasonal figure only}
  \item{trend}{The trend component}
  \item{random}{The remainder part}
  \item{type}{The value of \code{type}}
}
\author{
  David Meyer \email{David.Meyer@wu-wien.ac.at}
}
\seealso{\code{\link{stl}}}

\examples{
m <- decompose(co2)
m$figure
plot(m)
}
\keyword{ts}
