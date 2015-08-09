% File src/library/stats/man/plot.stepfun.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{plot.stepfun}
\alias{plot.stepfun}
\alias{lines.stepfun}
\title{Plot Step Functions}
\description{
  Method of the generic \code{\link{plot}} for \code{\link{stepfun}}
  objects and utility for plotting piecewise constant functions.
}
\usage{
\method{plot}{stepfun}(x, xval, xlim, ylim = range(c(y, Fn.kn)),
     xlab = "x", ylab = "f(x)", main = NULL,
     add = FALSE, verticals = TRUE, do.points = (n < 1000),
     pch = par("pch"), col = par("col"),
     col.points = col, cex.points = par("cex"),
     col.hor = col, col.vert = col,
     lty = par("lty"), lwd = par("lwd"), \dots)

\method{lines}{stepfun}(x, \dots)
}
\arguments{
  \item{x}{an \R object inheriting from \code{"stepfun"}.}
  \item{xval}{numeric vector of abscissa values at which to evaluate
    \code{x}.  Defaults to \code{\link{knots}(x)} restricted to \code{xlim}.}
  \item{xlim, ylim}{limits for the plot region: see
    \code{\link{plot.window}}.  Both have sensible defaults if omitted.}
  \item{xlab, ylab}{labels for x and y axis.}
  \item{main}{main title.}
  \item{add}{logical; if \code{TRUE} only \emph{add} to an existing plot.}
  \item{verticals}{logical;  if \code{TRUE}, draw vertical lines at steps.}
  \item{do.points}{logical;  if \code{TRUE}, also draw points at the
    (\code{xlim} restricted) knot locations.  Default is true, for
    sample size \eqn{< 1000}.}% n = 1 + 'sample size'
  \item{pch}{character; point character if \code{do.points}.}
  \item{col}{default color of all points and lines.}
  \item{col.points}{character or integer code; color of points if
    \code{do.points}.}
  \item{cex.points}{numeric; character expansion factor if \code{do.points}.}
  \item{col.hor}{color of horizontal lines.}
  \item{col.vert}{color of vertical lines.}
  \item{lty, lwd}{line type and thickness for all lines.}
  \item{\dots}{further arguments of \code{\link{plot}(.)}, or if\code{(add)}
    \code{\link{segments}(.)}.}
}
\value{
  A list with two components
  \item{t}{abscissa (x) values, including the two outermost ones.}
  \item{y}{y values \sQuote{in between} the \code{t[]}.}
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}, 1990,
    1993; ported to \R, 1997.}
\seealso{\code{\link{ecdf}} for empirical distribution functions as
  special step functions,
  \code{\link{approxfun}} and \code{\link{splinefun}}.
}
\examples{
require(graphics)

y0 <- c(1,2,4,3)
sfun0  <- stepfun(1:3, y0, f = 0)
sfun.2 <- stepfun(1:3, y0, f = .2)
sfun1  <- stepfun(1:3, y0, right = TRUE)

tt <- seq(0, 3, by = 0.1)
op <- par(mfrow = c(2,2))
plot(sfun0); plot(sfun0, xval = tt, add = TRUE, col.hor = "bisque")
plot(sfun.2);plot(sfun.2, xval = tt, add = TRUE, col = "orange") # all colors
plot(sfun1);lines(sfun1, xval = tt, col.hor = "coral")
##-- This is  revealing :
plot(sfun0, verticals = FALSE,
     main = "stepfun(x, y0, f=f)  for f = 0, .2, 1")
for(i in 1:3)
  lines(list(sfun0, sfun.2, stepfun(1:3, y0, f = 1))[[i]], col = i)
legend(2.5, 1.9, paste("f =", c(0, 0.2, 1)), col = 1:3, lty = 1, y.intersp = 1)
par(op)

# Extend and/or restrict 'viewport':
plot(sfun0, xlim = c(0,5), ylim = c(0, 3.5),
     main = "plot(stepfun(*), xlim= . , ylim = .)")

##-- this works too (automatic call to  ecdf(.)):
plot.stepfun(rt(50, df = 3), col.vert = "gray20")
}
\keyword{hplot}
