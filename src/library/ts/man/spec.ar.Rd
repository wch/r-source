\name{spec.ar}
\alias{spec.ar}
\title{Estimate Spectral Density of a Time Series from AR Fit}
\usage{
spec.ar(x, n.freq, order=NULL, plot=TRUE, na.action, ...)
}
\arguments{
 \item{x}{A univariate (not yet:or multivariate) time series or the
     result of a fit by \code{\link{ar}}.}
 \item{n.freq}{The number of points at which to plot.}
 \item{order}{The order of the AR model to be fitted. If omitted, the
     order is chosen by AIC.}
 \item{plot}{Plot the periodogram?}
 \item{na.action}{NA action function.}
 \item{\dots}{Graphical arguments passed to \code{\link{plot.spec}}.}
}
\description{
    Fits an R model to \code{x} (or uses the existing fit) and computes
    (and by default plots) the spectral density of the fitted model.
}
\value{
    An object of class \code{\link{spec}}.
  }
\author{B.D. Ripley}
\seealso{\code{\link{ar}}, \code{\link{spectrum}}.}
\note{The multivariate case is not yet implemented.}
\examples{
data(lh)
spec.ar(lh)
data(deaths)
spec.ar(deaths)
}
\keyword{ts}
