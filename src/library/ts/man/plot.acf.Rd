\name{plot.acf}
\alias{plot.acf}
\title{Plotting Autocovariance and Autocorrelation Functions}
\description{
  Plotting method for objects of class \code{"acf"}.
}
\synopsis{
plot.acf(x, ci = 0.95, type = "h",
     xlab = "Lag", ylab = NULL, ylim = NULL, main = NULL,
     ci.col="blue", ci.type=c("white", "ma"), \dots)
}
\usage{
plot(x, ci = 0.95, type = "h",
     xlab = "Lag", ylab = NULL, ylim = NULL, main = NULL,
     ci.col="blue", ci.type=c("white", "ma"), \dots)
}
\arguments{
  \item{x}{an object of class \code{"acf"}.}
  \item{ci}{coverage probability for confidence interval.  Plotting of
    the confidence interval is suppressed if \code{ci} is zero or
    negative.}
  \item{type}{the type of plot to be drawn, default to histogram like
    vertical lines.}
  \item{xlab}{the x label of the plot.}  
  \item{ylab}{the y label of the plot.}
  \item{ylim}{numeric of length 2 giving the y limits for the plot.}
  \item{main}{overall title for the plot.}
  \item{ci.col}{colour to plot the confidence interval lines.}
  \item{ci.type}{should the confidence limits assume a white noise
    input or for lag \eqn{k} an MA(\eqn{k-1}) input?}
  \item{\dots}{graphics parameters to be passed to the plotting
    routines.}
}
\note{
  The confidence interval plotted in \code{plot.acf} is based on an
  \emph{uncorrelated} series and should be treated with appropriate
  caution.  Using \code{ci.type = "ma"} may be less potentially
  misleading.
}
\seealso{
  \code{\link{acf}}
}
\keyword{hplot}
\keyword{ts}
