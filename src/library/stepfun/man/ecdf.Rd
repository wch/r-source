\name{ecdf}
\title{Empirical Cumulative Distribution Function}
\usage{
ecdf(x)
plot(ecdf(x), verticals = FALSE, col.01line = "gray70", \dots)
}
\alias{ecdf}
\alias{print.ecdf}
\alias{summary.ecdf}
\alias{plot.ecdf}
\arguments{\item{x}{numeric vector with the ``observations''.}
}
\description{
  For observations \code{x}, \code{ecdf(x)} returns the
  e.c.d.f. (empirical cumulative distribution function), i.e.
  the function \eqn{F_n}{Fn}.  Given  the observations,
  \code{x}\eqn{= (}\eqn{x_1,x_2}{x1,x2},\ldots\eqn{x_n)}{xn)},
  \eqn{F_n}{Fn} is the fraction of observations less or equal to \eqn{t},
  i.e.,
  \deqn{F_n(t) = \#\{x_i\le t\}\ / n
               = \frac1 n\sum_{i=1}^n \mathbf{1}_{[x_i \le t]}.}{
    Fn(t) = \#\{x_i\le t\} /n  =  1/n sum(i=1,..,n) Indicator(xi <= t).}

  The function \code{plot.ecdf} which implements the \code{\link{plot}}
  method for \code{ecdf} objects, is implemented via a call to
  \code{\link{plot.stepfun}}.
}
\value{
  An \R function \eqn{F_n}{Fn} of class \code{"ecdf"}, inheriting from the
  \code{"\link{stepfun}"} class.
}
\author{
  Martin Maechler, \email{maechler@stat.math.ethz.ch}.
}
\seealso{\code{\link{stepfun}}, the more general class of step functions,
  \code{\link{approxfun}} and \code{\link{splinefun}}.
}
\examples{
##-- Simple didactical  ecdf  example:
Fn <- ecdf(rnorm(12))
Fn; summary(Fn)
12*Fn(knots(Fn)) == 1:12 ## == 1:12  iff there are no ties !

y <- round(rnorm(12),1); y[3] <- y[1]
Fn12 <- ecdf(y)
Fn12
print(knots(Fn12), dig=2)
12*Fn12(knots(Fn12)) ## ~= 1:12  if there where no ties

summary(Fn12)
summary.stepfun(Fn12)
print(ls.Fn12 <- ls(env= environment(Fn12)))
##[1] "f"      "method" "n"      "x"      "y"      "yleft"  "yright"

12 * Fn12((-20:20)/10)

###----------------- Plotting --------------------------

op <- par(mfrow=c(3,1), mgp=c(1.5, 0.8,0), mar= .1+c(3,3,2,1))

F10 <- ecdf(rnorm(10))
summary(F10)

plot(F10)
plot(F10, verticals= TRUE, do.p = F)

plot(Fn12)# , lwd=2) dis-regarded
xx <- unique(sort(c(seq(-3,2, length=201), knots(Fn12))))
lines(xx, Fn12(xx), col='blue')
abline(v=knots(Fn12),lty=2,col='gray70')

plot(xx, Fn12(xx), type='b', cex=.1)#- plot.default
plot(Fn12, col.h='red', add= TRUE)  #- plot method
abline(v=knots(Fn12),lty=2,col='gray70')
plot(Fn12, verticals=T, col.p='blue', col.h='red',col.v='bisque')
par(op)

##-- this works too (automatic call to  ecdf(.)):
plot.ecdf(rnorm(24))
}
\keyword{iplot}
\keyword{hplot}

