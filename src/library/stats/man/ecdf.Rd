% File src/library/stats/man/ecdf.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Copyright 2002-2011 The R Foundation
% Distributed under GPL 2 or later

\name{ecdf}
\alias{ecdf}
\alias{plot.ecdf}
\alias{print.ecdf}
\alias{summary.ecdf}
\alias{quantile.ecdf}
\title{Empirical Cumulative Distribution Function}
\usage{
ecdf(x)

\method{plot}{ecdf}(x, \dots, ylab="Fn(x)", verticals = FALSE,
     col.01line = "gray70", pch = 19)

\method{print}{ecdf}(x, digits= getOption("digits") - 2, \dots)

\method{summary}{ecdf}(object, \dots)
\method{quantile}{ecdf}(x, \dots)
}
\arguments{
  \item{x, object}{numeric vector of the observations for \code{ecdf};  for
    the methods, an object inheriting from class \code{"ecdf"}.}
  \item{\dots}{arguments to be passed to subsequent methods, e.g.,
    \code{\link{plot.stepfun}} for the \code{plot} method.}
  \item{ylab}{label for the y-axis.}
  \item{verticals}{see \code{\link{plot.stepfun}}.}
  \item{col.01line}{numeric or character specifying the color of the
    horizontal lines at y = 0 and 1, see \code{\link{colors}}.}
  \item{pch}{plotting character.}
  \item{digits}{number of significant digits to use, see
    \code{\link{print}}.}
}
\description{
  Compute an empirical cumulative distribution function, with several
  methods for plotting, printing and computing with such an
  \dQuote{ecdf} object.
}
\details{
  The e.c.d.f. (empirical cumulative distribution function)
  \eqn{F_n}{Fn} is a step function with jumps \eqn{i/n} at
  observation values, where \eqn{i} is the number of tied observations
  at that value.  Missing values are ignored.

  For observations
  \code{x}\eqn{= (}\eqn{x_1,x_2}{x1,x2}, \ldots \eqn{x_n)}{xn)},
  \eqn{F_n}{Fn} is the fraction of observations less or equal to \eqn{t},
  i.e.,
  \deqn{F_n(t) = \#\{x_i\le t\}\ / n
               = \frac1 n\sum_{i=1}^n \mathbf{1}_{[x_i \le t]}.}{
    Fn(t) = #{xi <= t}/n  =  1/n sum(i=1,n) Indicator(xi <= t).}

  The function \code{plot.ecdf} which implements the \code{\link{plot}}
  method for \code{ecdf} objects, is implemented via a call to
  \code{\link{plot.stepfun}}; see its documentation.
}
\value{
  For \code{ecdf}, a function of class \code{"ecdf"}, inheriting from the
  \code{"\link{stepfun}"} class, and hence inheriting a
  \code{\link{knots}()} method.

  For the \code{summary} method, a summary of the knots of \code{object}
  with a \code{"header"} attribute.

  The \code{\link{quantile}(obj, ...)} method computes the same quantiles as
  \code{quantile(x, ...)} would where \code{x} is the original sample.
}
\author{
  Martin Maechler, \email{maechler@stat.math.ethz.ch}.\cr
  Corrections by R-core.
}
\seealso{\code{\link{stepfun}}, the more general class of step functions,
  \code{\link{approxfun}} and \code{\link{splinefun}}.
}
\examples{
##-- Simple didactical  ecdf  example :
x <- rnorm(12)
Fn <- ecdf(x)
Fn     # a *function*
Fn(x)  # returns the percentiles for x
tt <- seq(-2, 2, by = 0.1)
12 * Fn(tt) # Fn is a 'simple' function {with values k/12}
summary(Fn)
##--> see below for graphics
knots(Fn)  # the unique data values {12 of them if there were no ties}

y <- round(rnorm(12), 1); y[3] <- y[1]
Fn12 <- ecdf(y)
Fn12
knots(Fn12) # unique values (always less than 12!)
summary(Fn12)
summary.stepfun(Fn12)

## Advanced: What's inside the function closure?
print(ls.Fn12 <- ls(environment(Fn12)))
##[1] "f"  "method"  "n"  "x"  "y"  "yleft"  "yright"
utils::ls.str(environment(Fn12))
stopifnot(all.equal(quantile(Fn12), quantile(y)))

###----------------- Plotting --------------------------
require(graphics)

op <- par(mfrow = c(3, 1), mgp = c(1.5, 0.8, 0), mar =  .1+c(3,3,2,1))

F10 <- ecdf(rnorm(10))
summary(F10)

plot(F10)
plot(F10, verticals = TRUE, do.points = FALSE)

plot(Fn12 , lwd = 2) ; mtext("lwd = 2", adj = 1)
xx <- unique(sort(c(seq(-3, 2, length = 201), knots(Fn12))))
lines(xx, Fn12(xx), col = "blue")
abline(v = knots(Fn12), lty = 2, col = "gray70")

plot(xx, Fn12(xx), type = "o", cex = .1)  #- plot.default {ugly}
plot(Fn12, col.hor = "red", add =  TRUE)  #- plot method
abline(v = knots(Fn12), lty = 2, col = "gray70")
## luxury plot
plot(Fn12, verticals = TRUE, col.points = "blue",
     col.hor = "red", col.vert = "bisque")

##-- this works too (automatic call to  ecdf(.)):
plot.ecdf(rnorm(24))
title("via  simple  plot.ecdf(x)", adj = 1)

par(op)
}
\keyword{dplot}
\keyword{hplot}
