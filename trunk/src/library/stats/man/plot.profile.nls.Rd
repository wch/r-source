\name{plot.profile.nls}
\alias{plot.profile.nls}
\title{Plot a profile.nls Object}
\description{
  Displays a series of plots of the profile t function and interpolated
  confidence intervals for the parameters in a nonlinear regression
  model that has been fit with \code{nls} and profiled with
  \code{profile.nls}.
}
\usage{
\method{plot}{profile.nls}(x, levels, conf= c(99, 95, 90, 80, 50)/100,
     nseg = 50, absVal =TRUE, \dots)
}
\arguments{
 \item{x}{an object of class \code{"profile.nls"} }
 \item{levels}{levels, on the scale of the absolute value of a t
     statistic, at which to interpolate intervals.  Usually \code{conf}
     is used instead of giving \code{levels} explicitly.}
 \item{conf}{a numeric vector of confidence levels for profile-based
     confidence intervals on the parameters.  Defaults to \code{c(0.99,
	 0.95, 0.90, 0.80, 0.50).}}
 \item{nseg}{an integer value giving the number of segments to use in
     the spline interpolation of the profile t curves. Defaults to 50.}
 \item{absVal}{a logical value indicating whether or not the plots
     should be on the scale of the absolute value of the profile t.
     Defaults to \code{TRUE}.} 
 \item{\dots}{other arguments to the \code{plot} function can be passed here.}
}
\references{
  Bates, D.M. and Watts, D.G. (1988),
  \emph{Nonlinear Regression Analysis and Its Applications},
  Wiley (chapter 6)
}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{
  \code{\link{nls}},
  \code{\link{profile}},
  \code{\link{profile.nls}}
}
\examples{
# obtain the fitted object
fm1 <- nls(demand ~ SSasympOrig( Time, A, lrc ), data = BOD)
# get the profile for the fitted model
pr1 <- profile( fm1 )
opar <- par(mfrow = c(2,2), oma = c(1.1, 0, 1.1, 0), las = 1)
plot(pr1, conf = c(95, 90, 80, 50)/100)
plot(pr1, conf = c(95, 90, 80, 50)/100, absVal = FALSE)
mtext("Confidence intervals based on the profile sum of squares",
      side = 3, outer = TRUE)
mtext("BOD data - confidence levels of 50\%, 80\%, 90\% and 95\%",
      side = 1, outer = TRUE)
par(opar)
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
