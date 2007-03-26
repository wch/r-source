\name{plot.isoreg}
\alias{plot.isoreg}
\alias{lines.isoreg}
%\alias{print.isoreg}
\title{Plot Method for isoreg  Objects}
\description{
  The \code{\link{plot}} and \code{\link{lines}} method for
  \R objects of class \code{\link{isoreg}}.
}
\usage{
\method{plot}{isoreg}(x, plot.type = c("single", "row.wise", "col.wise"),
      main = paste("Isotonic regression", deparse(x$call)),
      main2 = "Cumulative Data and Convex Minorant",
      xlab = "x0", ylab = "x$y",
      par.fit = list(col = "red", cex = 1.5, pch = 13, lwd = 1.5),
      mar = if (both) 0.1 + c(3.5, 2.5, 1, 1) else par("mar"),
      mgp = if (both) c(1.6, 0.7, 0) else par("mgp"),
      grid = length(x$x) < 12, \dots)
\method{lines}{isoreg}(x, col = "red", lwd = 1.5,
       do.points = FALSE, cex = 1.5, pch = 13, \dots)

%\method{print}{isoreg}(x, digits = getOption("digits"), \dots)
}
\arguments{
  \item{x}{an \code{\link{isoreg}} object.}
  \item{plot.type}{character indicating which type of plot is desired.
    The first (default) only draws the data and the fit, where the
    others add a plot of the cumulative data and fit.}
  \item{main}{main title of plot, see \code{\link{title}}.}
  \item{main2}{title for second (cumulative) plot.}
  \item{xlab, ylab}{x- and y- axis annotation.}
  \item{par.fit}{a \code{\link{list}} of arguments (for
    \code{\link{points}} and \code{\link{lines}}) for drawing the fit.}
  \item{mar, mgp}{graphical parameters, see \code{\link{par}}, mainly
    for the case of two plots.}
  \item{grid}{logical indicating if grid lines should be drawn.  If
    true, \code{\link{grid}()} is used for the first plot, where as
    vertical lines are drawn at \dQuote{touching} points for the
    cumulative plot.}
  \item{do.points}{for \code{lines()}: logical indicating if the step
    points should be drawn as well (and as they are drawn in \code{plot()}).}
  \item{col, lwd, cex, pch}{graphical arguments for \code{lines()},
    where \code{cex} and \code{pch} are only used when \code{do.points}
    is \code{TRUE}.}
  \item{\dots}{further arguments passed to and from methods.}
%  \item{digits}{integer indicating number of digits to use for printing.}
}
\seealso{\code{\link{isoreg}} for computation of \code{isoreg} objects.}
\examples{
example(isoreg) # for the examples there

plot(y3, main = "simple plot(.)  +  lines(<isoreg>)")
lines(ir3)

## 'same' plot as above, "proving" that only ranks of 'x' are important
plot(isoreg(2^(1:9), c(1,0,4,3,3,5,4,2,0)), plot.t = "row", log = "x")

plot(ir3, plot.type = "row", ylab = "y3")
plot(isoreg(y3 - 4), plot.t="r", ylab = "y3 - 4")
plot(ir4, plot.type = "ro",  ylab = "y4", xlab = "x = 1:n")

## experiment a bit with these (C-c C-j):
plot(isoreg(sample(9),  y3), plot.type="row")
plot(isoreg(sample(9),  y3), plot.type="col.wise")

plot(ir <- isoreg(sample(10), sample(10, replace = TRUE)), plot.t = "r")
}
\keyword{hplot}
\keyword{print}
