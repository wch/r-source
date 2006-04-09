\name{lowess}
\title{Scatter Plot Smoothing}
\usage{
lowess(x, y = NULL, f = 2/3, iter = 3,
       delta = 0.01 * diff(range(xy$x[o])))
}
\alias{lowess}
\arguments{
  \item{x, y}{vectors giving the coordinates of the points in the scatter plot.
    Alternatively a single plotting structure can be specified.}
  \item{f}{the smoother span. This gives the proportion of points in
    the plot which influence the smooth at each value.
    Larger values give more smoothness.}
  \item{iter}{the number of robustifying iterations which should be
    performed.
    Using smaller values of \code{iter} will make \code{lowess} run faster.}
  \item{delta}{values of \code{x} which lie within \code{delta}
    of each other are replaced by a single value in the output from
    \code{lowess}.  Defaults to 1/100th of the range of \code{x}.}
}
\description{
  This function performs the computations for the
  \emph{LOWESS} smoother (see the reference below).
  \code{lowess} returns a list containing components
  \code{x} and \code{y} which give the coordinates of the smooth.
  The smooth should be added to a plot of the original
  points with the function \code{lines}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Cleveland, W. S. (1979)
  Robust locally weighted regression and smoothing scatterplots.
  \emph{J. Amer. Statist. Assoc.} \bold{74}, 829--836.

  Cleveland, W. S. (1981)
  LOWESS: A program for smoothing scatterplots by robust locally weighted
  regression. \emph{The American Statistician}, \bold{35}, 54.
}
\seealso{\code{\link{loess}}, a newer
  formula based version of \code{lowess} (with different defaults!).
}
\examples{
plot(cars, main = "lowess(cars)")
lines(lowess(cars), col = 2)
lines(lowess(cars, f=.2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)
}
\keyword{smooth}
