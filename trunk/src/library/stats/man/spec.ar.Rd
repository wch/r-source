\name{spec.ar}
\alias{spec.ar}
\title{Estimate Spectral Density of a Time Series from AR Fit}
\usage{
spec.ar(x, n.freq, order = NULL, plot = TRUE, na.action = na.fail,
        method = "yule-walker", \dots)
}
\arguments{
  \item{x}{A univariate (not yet:or multivariate) time series or the
    result of a fit by \code{\link{ar}}.}
  \item{n.freq}{The number of points at which to plot.}
  \item{order}{The order of the AR model to be fitted.  If omitted,
    the order is chosen by AIC.}
  \item{plot}{Plot the periodogram?}
  \item{na.action}{\code{NA} action function.}
  \item{method}{\code{method} for \code{ar} fit.}
  \item{\dots}{Graphical arguments passed to \code{\link{plot.spec}}.}
}
\description{
  Fits an AR model to \code{x} (or uses the existing fit) and computes
  (and by default plots) the spectral density of the fitted model.
}
\value{
  An object of class \code{"spec"}.
  The result is returned invisibly if \code{plot} is true.
}
\references{
  Thompson, D.J. (1990) Time series analysis of Holocene climate data.
  \emph{Phil. Trans. Roy. Soc. A} \bold{330}, 601--616.

  Venables, W.N. and Ripley, B.D. (2002) \emph{Modern Applied
    Statistics with S.} Fourth edition. Springer. (Especially
  page 402.)
}
\section{Warning}{
  Some authors, for example Thomson (1990), warn strongly
  that AR spectra can be misleading.
}

\seealso{\code{\link{ar}}, \code{\link{spectrum}}.}

\note{The multivariate case is not yet implemented.}

\examples{
spec.ar(lh)

spec.ar(ldeaths)
spec.ar(ldeaths, method="burg")
}
\keyword{ts}
