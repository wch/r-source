%%%% file modreg/man/smooth.spline.Rd
%%%% copyright (C) 1998 B. D. Ripley
%
\name{smooth.spline}
\title{Fit a Smoothing Spline}
\usage{
smooth.spline(x, y, w = rep(1, length(x)), df = 5, spar = 0,
              cv = FALSE, all.knots = FALSE, df.offset = 0, penalty = 1)
}
\alias{smooth.spline}
\alias{predict.smooth.spline.fit}
\alias{print.smooth.spline}
\arguments{
 \item{x}{a vector giving the values of the predictor variable, or  a
   list or a two-column matrix specifying x and y. }
 \item{y}{responses. If \code{y} is missing, the responses are assumed
   to be specified by \code{x}.}
 \item{w}{optional vector of weights}
 \item{df}{the desired equivalent number of degrees of freedom (trace of
     the smoother matrix).}
 \item{spar}{the coefficient \eqn{\lambda} of the integral of the squared
   second derivative in the fit (penalized log lik.) criterion.}
 \item{cv}{ordinary (\code{TRUE}) or `generalized' (\code{FALSE})
   cross-validation.}
 \item{all.knots}{if \code{TRUE}, all points in \code{x} are uses as
   knots. If \code{FALSE}, a suitably fine grid of knots is used.}
 \item{df.offset}{allows the degrees of freedom to be increased by
   \code{df.offset} in the GCV criterion.}
 \item{penalty}{the coefficient of the penalty for degrees of freedom
   in the GCV criterion.}
}
\description{Fits a cubic smoothing spline to the supplied data.
}
\details{
  The \code{x} vector should contain at least ten distinct values.

  If \code{spar} is missing or 0, the value of \code{df} is used to
  determine the degree of smoothing. If both are missing, leave-one-out
  cross-validation is used to determine \eqn{\lambda}.

  The `generalized' cross-validation method will work correctly when
  there are duplicated points in \code{x}.  However, it is ambiguous what
  leave-one-out cross-validation means with duplicated points, and the
  internal code uses an approximation that involves leaving out groups
  of duplicated points.  \code{cv=TRUE} is best avoided in that case.
}
\value{
  An object of class \code{"smooth.spline"} with components
  \item{x}{the distinct \code{x} values in increasing order.}
  \item{y}{the fitted values corresponding to \code{x}.}
  \item{w}{the weights used at the unique values of \code{x}.}
  \item{yin}{the y values used at the unique \code{y} values.}
  \item{lev}{leverages, the diagonal values of the smoother matrix.}
  \item{cv.crit}{(generalized) cross-validation score.}
  \item{pen.crit}{penalized criterion}
  \item{df}{equivalent degrees of freedom used.}
  \item{spar}{the value of \eqn{\lambda} chosen.}
  \item{fit}{list for use by \code{predict.smooth.spline}.}
  \item{call}{}
}
\author{B.D. Ripley}
\seealso{\code{\link{predict.smooth.spline}}}
\examples{
data(cars)
attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)
## This example has duplicate points, so avoid cv=TRUE
\testonly{
  all(cars.spl $ w == table(speed)) # TRUE (weights = multiplicities)
  str(cars.spl, digits=5, vec.len=6)
  cars.spl$fit
}
lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df=10), lty=2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg='bisque')
detach()
}
\keyword{smooth}
