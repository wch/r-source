\name{ecdf}
\alias{ecdf}
\alias{plot.ecdf}
\alias{print.ecdf}
\title{Empirical Cumulative Distribution Function}
\usage{
ecdf(x)

\method{plot}{ecdf}(x, \dots, ylab="Fn(x)", verticals = FALSE,
     col.01line = "gray70")

\method{print}{ecdf}(x, digits= getOption("digits") - 2, \dots)
}
\arguments{
  \item{x}{numeric vector of \dQuote{observations} in \code{ecdf};  for
    the methods, an object of class \code{"ecdf"}, typically.}
  \item{\dots}{arguments to be passed to subsequent methods, i.e.,
    \code{\link{plot.stepfun}} for the \code{plot} method.}
  \item{ylab}{label for the y-axis.}
  \item{verticals}{see \code{\link{plot.stepfun}}.}
  \item{col.01line}{numeric or character specifying the color of the
    horizontal lines at y = 0 and 1, see \code{\link{colors}}.}
  \item{digits}{number of significant digits to use, see
    \code{\link{print}}.}
}
\description{
  Compute or plot an empirical cumulative distribution function.
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
    Fn(t) = #\{x_i \le t\} / n  =  1/n sum(i=1,n) Indicator(xi <= t).}

  The function \code{plot.ecdf} which implements the \code{\link{plot}}
  method for \code{ecdf} objects, is implemented via a call to
  \code{\link{plot.stepfun}}; see its documentation.
}
\value{
  For \code{ecdf}, a function of class \code{"ecdf"}, inheriting from the
  \code{"\link{stepfun}"} class.
}
\author{
  Martin Maechler, \email{maechler@stat.math.ethz.ch}.\cr
  Corrections by R-core.
}
\seealso{\code{\link{stepfun}}, the more general class of step functions,
  \code{\link{approxfun}} and \code{\link{splinefun}}.
}
\examples{
##-- Simple didactical  ecdf  example:
Fn <- ecdf(rnorm(12))
Fn
tt <- seq(-2,2, by = 0.1)
12* Fn(tt) # Fn is a `simple' function {with values k/12}
summary(Fn)
knots(Fn)# the unique data values {12 of them if there were no ties}

y <- round(rnorm(12),1); y[3] <- y[1]
Fn12 <- ecdf(y)
Fn12
knots(Fn12)# unique values (always less than 12!)
summary(Fn12)
summary.stepfun(Fn12)

## Advanced: What's inside the function closure?
print(ls.Fn12 <- ls(environment(Fn12)))
##[1] "f"  "method"  "n"  "x"  "y"  "yleft"  "yright"
utils::ls.str(environment(Fn12))


###----------------- Plotting --------------------------
require(graphics)

op <- par(mfrow=c(3,1), mgp=c(1.5, 0.8,0), mar= .1+c(3,3,2,1))

F10 <- ecdf(rnorm(10))
summary(F10)

plot(F10)
plot(F10, verticals= TRUE, do.points = FALSE)

plot(Fn12 , lwd = 2) ; mtext("lwd = 2", adj=1)
xx <- unique(sort(c(seq(-3, 2, length=201), knots(Fn12))))
lines(xx, Fn12(xx), col='blue')
abline(v=knots(Fn12),lty=2,col='gray70')

plot(xx, Fn12(xx), type='o', cex=.1)#- plot.default {ugly}
plot(Fn12, col.hor='red', add= TRUE)  #- plot method
abline(v=knots(Fn12),lty=2,col='gray70')
## luxury plot
plot(Fn12, verticals=TRUE, col.points='blue',
     col.hor='red', col.vert='bisque')

##-- this works too (automatic call to  ecdf(.)):
plot.ecdf(rnorm(24))
title("via  simple  plot.ecdf(x)", adj=1)

par(op)
}
\keyword{dplot}
\keyword{hplot}

