\name{plot.spec}
\alias{plot.spec}
\alias{plot.spec.coherency}
\alias{plot.spec.phase}
\title{Plotting Spectral Densities}
\description{
  Plotting method for objects of class \code{"spec"}.  For multivariate
  time series it plots the marginal spectra of the series or pairs plots
  of the coherency and phase of the cross-spectra.
}
\synopsis{
plot.spec(x, add = FALSE, ci = 0.95, log = c("yes", "dB", "no"), 
    xlab = "frequency", ylab = NULL, type = "l",
    ci.col = "blue", main = NULL, sub = NULL,
    plot.type = c("marginal", "coherency", "phase"), \dots) 
plot.spec.phase(x, ci = 0.95, xlab = "frequency",
    ylab = "phase", ylim = c(-pi, pi), type = "l",
    main = NULL, ci.lty = 3, ci.col = "blue", \dots) 
plot.spec.coherency(x, ci = 0.95, xlab = "frequency",
    ylab = "squared coherency", ylim = c(0, 1), type = "l",
    main = NULL, ci.lty = 3, ci.col = "blue", \dots)
}
\usage{
plot(x, add = FALSE, ci = 0.95, log = c("yes", "dB", "no"),
     xlab = "frequency", ylab = NULL, type = "l", ci.col = "blue",
     main = NULL, sub = NULL,
     plot.type = c("marginal", "coherency", "phase"),
     ci.lty = 3, \dots)
}
\arguments{
  \item{x}{an object of class \code{"spec"}.}
  \item{add}{logical.  If \code{TRUE}, add to already existing plot.}
  \item{ci}{Coverage probability for confidence interval.  Plotting of
    the confidence bar is omitted unless \code{ci} is strictly
    positive.}
  \item{log}{If \code{"dB"}, plot on log10 (decibel) scale (as S-PLUS),
    otherwise use conventional log scale or linear scale.  Logical
    values are also accepted.  The default is \code{"yes"} unless
    \code{options(ts.S.compat = TRUE)} has been set, when it is
    \code{"dB"}.}
  \item{xlab}{the x label of the plot.}  
  \item{ylab}{the y label of the plot.}
  \item{type}{the type of plot to be drawn, defaults to lines.}
  \item{ci.col}{Colour for plotting confidence bar or confidence
    intervals for coherency and phase.}
  \item{main}{overall title for the plot.}
  \item{sub}{a sub title for the plot.}
  \item{plot.type}{For multivariate time series, the type of plot
    required.  Only the first character is needed.}
  \item{ci.lty}{line type for confidence intervals for coherency and
    phase.}
  \item{\dots}{Further graphical parameters.}
}
\seealso{
  \code{\link{spectrum}}
}
\keyword{hplot}
\keyword{ts}
