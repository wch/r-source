\name{spectrum}
\alias{spectrum}
\alias{plot.spec}
\alias{spec}
\title{Spectral Density Estimation}
\usage{
spectrum(x, method=c("pgram","ar"), plot = TRUE, ...)
plot.spec(spec.obj, add=FALSE, ci=0.95, log=TRUE, ...)
}
\arguments{
 \item{x}{A time series.}

 \item{method}{String specifying the method used to estimate the spectral 
    density. Allowed methods are "pgram" (the default) and "ar".}

\item{plot}{logical. If \code{TRUE} then the spectral density is plotted.}

\item{\dots}{Further arguments to specific spec methods or \code{plot.spec}.}

\item{spec.obj}{An object of class \code{spec}.}

\item{add}{logical. If \code{TRUE} then lines are added to the
    existing plot.}

\item{ci}{Coverage probability for confidence interval. Plotting of the
    confidence bar is suppressed if \code{ci} <= 0.}

\item{log}{If true, plot on log10 (decibel) scale.}

\item{\dots}{Further graphical parameters.}
}
\description{
The \code{spectrum} function estimates the spectral density of a
time series. This is just a wrapper function which calls the methods
\code{\link{spec.pgram}} and \code{\link{spec.ar}}.

The generic function \code{plot} has a method for \code{spec} objects.
}
\value{
  An object of class \code{spec}, which is a list containing at
  least the following elements:
  \item{freq}{vector of fourier frequencies at which the spectral
     density is estimated.}
  \item{spec}{Vector (for univarite series) or matrix (for multivariate
     series) of estimates of the spectral density at frequencies
     corresponding to \code{freq}.}
  \item{coh}{\code{NULL} for univariate series. For multivariate time
     series, a matrix containing the squared coherency between different
     series. Column \eqn{ i + (j - 1) * (j - 2)/2} of \code{coh}
     contains the squared coherency between columns \eqn{i} and \eqn{j}
     of \code{x}, where \eqn{i>j}.}
  \item{phase}{\code{NULL} for univariate series. For multivariate
     time series a matrix containing the cross-spectrum phase between
     different series. The format is the same as \code{coh}.}
  \item{series}{The name of the time series.}
  \item{method}{The method used to calculate the spectrum.}

}
\note{
The default plot for \code{spec} objects is quite complex, including an
error bar and default title, subtitle and axis labels. The defaults can
all be overridden by supplying the appropriate graphical parameters.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Martyn Plummer, B.D. Ripley}
\seealso{\code{\link{spec.pgram}}}

\examples{
## spec.pgram
par(mfrow=c(2,2))
data(lh)
spectrum(lh)
spectrum(lh, spans=3)
spectrum(lh, spans=c(3,3))
spectrum(lh, spans=c(3,5))

data(deaths)
spectrum(deaths)
spectrum(deaths, spans=c(3,3))
spectrum(deaths, spans=c(3,5))
spectrum(deaths, spans=c(5,7))

## spec.ar
spectrum(lh, method="ar")
spectrum(deaths, method="ar")
}
\keyword{ts}
