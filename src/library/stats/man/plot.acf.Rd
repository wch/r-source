% File src/library/stats/man/plot.acf.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{plot.acf}
\alias{plot.acf}
\title{Plot Autocovariance and Autocorrelation Functions}
\description{
  Plot method for objects of class \code{"acf"}.
}
\usage{
\method{plot}{acf}(x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
     ylim = NULL, main = NULL,
     ci.col = "blue", ci.type = c("white", "ma"),
     max.mfrow = 6, ask = Npgs > 1 && dev.interactive(),
     mar = if(nser > 2) c(3,2,2,0.8) else par("mar"),
     oma = if(nser > 2) c(1,1.2,1,1) else par("oma"),
     mgp = if(nser > 2) c(1.5,0.6,0) else par("mgp"),
     xpd = par("xpd"),
     cex.main = if(nser > 2) 1 else par("cex.main"),
     verbose = getOption("verbose"),
     \dots)
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
    input or for lag \eqn{k} an MA(\eqn{k-1}) input?   Can be abbreviated.}
  \item{max.mfrow}{positive integer; for multivariate \code{x}
    indicating how many rows and columns of plots should be put on one
    page, using \code{\link{par}(mfrow = c(m,m))}.}
  \item{ask}{logical; if \code{TRUE}, the user is asked before a new
    page is started.}
  \item{mar, oma, mgp, xpd, cex.main}{graphics parameters as in
    \code{\link{par}(*)}, by default adjusted to use smaller than
    default margins for multivariate \code{x} only.
  }
  \item{verbose}{logical.  Should \R report extra information on
    progress?}
  \item{\dots}{graphics parameters to be passed to the plotting
    routines.}
}
\note{
  The confidence interval plotted in \code{plot.acf} is based on an
  \emph{uncorrelated} series and should be treated with appropriate
  caution.  Using \code{ci.type = "ma"} may be less potentially
  misleading.
}
\seealso{\code{\link{acf}} which calls \code{plot.acf} by default.
}
\examples{
require(graphics)

%%-- FIXME: use less trivial data :
z4  <- ts(matrix(rnorm(400), 100, 4), start = c(1961, 1), frequency = 12)
z7  <- ts(matrix(rnorm(700), 100, 7), start = c(1961, 1), frequency = 12)
acf(z4)
acf(z7, max.mfrow = 7)   # squeeze onto 1 page
acf(z7) # multi-page
}
\keyword{hplot}
\keyword{ts}
