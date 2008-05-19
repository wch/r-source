% File src/library/stats/man/decompose.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

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
  \item{type}{The type of seasonal component. Can be abbreviated.}
  \item{filter}{A vector of filter coefficients in reverse time order (as for
    AR or MA coefficients), used for filtering out the seasonal
    component. If \code{NULL}, a moving average with symmetric window is
  performed.}
}
\details{
  The additive model used is:
  \deqn{Y_t = T_t + S_t + e_t}{Y[t] = T[t] + S[t] + e[t]}
  The multiplicative model used is:
  \deqn{Y_t = T_t\,S_t\, e_t}{Y[t] = T[t] * S[t] * e[t]}
  
  The function first determines the trend component using a moving
  average (if \code{filter} is \code{NULL}, a symmetric window with equal
  weights is used), and removes it from the time series.  Then, 
  the seasonal figure is computed by averaging, for each time unit,
  over all periods. The seasonal figure is then centered.
  Finally, the error component is determined by
  removing trend and seasonal figure (recycled as needed) from the orginal
  time series.
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
require(graphics)

m <- decompose(co2)
m$figure
plot(m)
}
\keyword{ts}
