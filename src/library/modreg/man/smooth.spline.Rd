% file modreg/man/smooth.spline.Rd
% copyright (C) 1998 B. D. Ripley
% copyright (C) 2001 The R Core Development Team
%
\name{smooth.spline}
\alias{smooth.spline}
\alias{predict.smooth.spline.fit}
\alias{print.smooth.spline}
\title{Fit a Smoothing Spline}
\description{
  Fits a cubic smoothing spline to the supplied data.
}
\usage{
smooth.spline(x, y, w = rep(1, length(x)), df = 5, spar = 0,
              cv = FALSE, all.knots = FALSE, df.offset = 0, penalty = 1)
}
\arguments{
 \item{x}{a vector giving the values of the predictor variable, or  a
  list or a two-column matrix specifying x and y. }
 \item{y}{responses. If \code{y} is missing, the responses are assumed
   to be specified by \code{x}.}
 \item{w}{optional vector of weights}
 \item{df}{the desired equivalent number of degrees of freedom (trace of
   the smoother matrix).}
 \item{spar}{smoothing parameter, typically in \eqn{(0,1]}. The
   coefficient \eqn{\lambda} of the integral of the squared second
   derivative in the fit (penalized log likelihood) criterion is a
   monotone function of \code{spar}, see the details below.}
 \item{cv}{ordinary (\code{TRUE}) or `generalized' (\code{FALSE})
   cross-validation.}
 \item{all.knots}{if \code{TRUE}, all points in \code{x} are uses as
   knots. If \code{FALSE}, a suitably fine grid of knots is used.}
 \item{df.offset}{allows the degrees of freedom to be increased by
   \code{df.offset} in the GCV criterion.}
 \item{penalty}{the coefficient of the penalty for degrees of freedom
   in the GCV criterion.}
}
\details{
  The \code{x} vector should contain at least ten distinct values.

  The computational \eqn{\lambda} used (as a function of
  \eqn{s=spar}{\code{spar}}) is
  \eqn{\lambda = r * 256^{3 s - 1}}{lambda = r * 256^(3*spar - 1)}
  where
  \eqn{r = tr(X' W^2 X) / tr(\Sigma)},
  \eqn{\Sigma} is the matrix given by
  \eqn{\Sigma_{ij} = \int B_i''(t) B_j''(t) dt}{%
    Sigma[i,j] = Integral B''[i](t) B''[j](t) dt},
  \eqn{X} is given by \eqn{X_{ij} = B_j(x_i)}{X[i,j] = B[j](x[i])},
  \eqn{W^2} is the diagonal matrix of scaled weights, \code{W =
    diag(w)/n} (i.e., the identity for default weights),
  and \eqn{B_k(.)}{B[k](.)} is the \eqn{k}-th B-spline.

  Note that with these definitions, \eqn{f_i = f(x_i)}, and the B-spline
  basis representation \eqn{f = X c} (i.e. \eqn{c} is
  the vector of spline coefficients), the penalized log likelihood is 
  \eqn{L = (y - f)' W^2 (y - f) + \lambda c' \Sigma c}, and hence
  \eqn{c} is the solution of the (ridge regression)
  \eqn{(X' W^2 X + \lambda \Sigma) c = X' W^2 y}.
 

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
  \item{call}{the matched call.}
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
\testonly{
  y2 <- c(1:5,7:3,2*(2:5))
  smooth.spline(y2)
  try(smooth.spline(y2, spar = 50)) # error (R 1.2.3) : spar `way too large'
}
}
\keyword{smooth}
