\name{smooth.spline}
\alias{smooth.spline}
%\alias{print.smooth.spline}% is not exported
\title{Fit a Smoothing Spline}
\description{
  Fits a cubic smoothing spline to the supplied data.
}
\usage{
smooth.spline(x, y = NULL, w = NULL, df, spar = NULL,
              cv = FALSE, all.knots = FALSE, nknots = NULL, keep.data = TRUE,
              df.offset = 0, penalty = 1, control.spar = list())
}
\arguments{
 \item{x}{a vector giving the values of the predictor variable, or  a
  list or a two-column matrix specifying x and y. }
 \item{y}{responses. If \code{y} is missing, the responses are assumed
   to be specified by \code{x}.}
 \item{w}{optional vector of weights of the same length as \code{x};
   defaults to all 1.}
 \item{df}{the desired equivalent number of degrees of freedom (trace of
   the smoother matrix).}
 \item{spar}{smoothing parameter, typically (but not necessarily) in
   \eqn{(0,1]}.  The coefficient \eqn{\lambda} of the integral of the
   squared second derivative in the fit (penalized log likelihood)
   criterion is a monotone function of \code{spar}, see the details
   below.}
 \item{cv}{ordinary (\code{TRUE}) or \dQuote{generalized} cross-validation
   (GCV) when \code{FALSE}.}
 \item{all.knots}{if \code{TRUE}, all distinct points in \code{x} are used as
   knots.  If \code{FALSE} (default), a subset of \code{x[]} is used,
   specifically \code{x[j]} where the \code{nknots} indices are evenly
   spaced in \code{1:n}, see also the next argument \code{nknots}.}
 \item{nknots}{integer giving the number of knots to use when
   \code{all.knots=FALSE}.  Per default, this is less than \eqn{n}, the
   number of unique \code{x} values for \eqn{n > 49}.}
 \item{keep.data}{logical specifiying if the input data should be kept
   in the result.  If \code{TRUE} (as per default), fitted values and
   residuals are available from the result.}
 \item{df.offset}{allows the degrees of freedom to be increased by
   \code{df.offset} in the GCV criterion.}
 \item{penalty}{the coefficient of the penalty for degrees of freedom
   in the GCV criterion.}
 \item{control.spar}{optional list with named components controlling the
   root finding when the smoothing parameter \code{spar} is computed,
   i.e., missing or \code{NULL}, see below.

   \bold{Note} that this is partly \emph{experimental} and may change
   with general spar computation improvements!

   \describe{
     \item{low:}{lower bound for \code{spar}; defaults to -1.5 (used to
       implicitly default to 0 in \R versions earlier than 1.4).}
     \item{high:}{upper bound for \code{spar}; defaults to +1.5.}
     \item{tol:}{the absolute precision (\bold{tol}erance) used; defaults
     to 1e-4 (formerly 1e-3).}
     \item{eps:}{the relative precision used; defaults to 2e-8 (formerly
       0.00244).}
     \item{trace:}{logical indicating if iterations should be traced.}
     \item{maxit:}{integer giving the maximal number of iterations;
       defaults to 500.}
   }
   Note that \code{spar} is only searched for in the interval
   \eqn{[low, high]}.
 }
}
\details{
  The \code{x} vector should contain at least four distinct values.
  \emph{Distinct} here means \dQuote{distinct after rounding to 6 significant
  digits}, i.e., \code{x} will be transformed to
  \code{unique(sort(signif(x, 6)))}, and \code{y} and \code{w} are
  pooled accordingly.

  The computational \eqn{\lambda} used (as a function of
  \eqn{s=spar}{\code{spar}}) is
  \eqn{\lambda = r * 256^{3 s - 1}}{lambda = r * 256^(3*spar - 1)}
  where
  \eqn{r = tr(X' W X) / tr(\Sigma)},
  \eqn{\Sigma} is the matrix given by
  \eqn{\Sigma_{ij} = \int B_i''(t) B_j''(t) dt}{%
    Sigma[i,j] = Integral B''[i](t) B''[j](t) dt},
  \eqn{X} is given by \eqn{X_{ij} = B_j(x_i)}{X[i,j] = B[j](x[i])},
  \eqn{W} is the diagonal matrix of weights (scaled such that
  its trace is \eqn{n}, the original number of observations)
  and \eqn{B_k(.)}{B[k](.)} is the \eqn{k}-th B-spline.

  Note that with these definitions, \eqn{f_i = f(x_i)}, and the B-spline
  basis representation \eqn{f = X c} (i.e., \eqn{c} is
  the vector of spline coefficients), the penalized log likelihood is
  \eqn{L = (y - f)' W (y - f) + \lambda c' \Sigma c}, and hence
  \eqn{c} is the solution of the (ridge regression)
  \eqn{(X' W X + \lambda \Sigma) c = X' W y}.

  If \code{spar} is missing or \code{NULL}, the value of \code{df} is used to
  determine the degree of smoothing.  If both are missing, leave-one-out
  cross-validation (ordinary or \dQuote{generalized} as determined by
  \code{cv}) is used to determine \eqn{\lambda}.
  Note that from the above relation,
%%  lam      = r * 256^(3s - 1)
%%  log(lam) = log(r) + (3s - 1) * log(256)
%% (log(lam) - log(r)) / log(256)  = 3s - 1
%% s = [1 +  {log(lam) - log(r)} / {8 log(2)} ] / 3
%%   = 1/3 + {log(lam) - log(r)} / {24 log(2)}
%%   = 1/3 - log(r)/{24 log(2)} +  log(lam) / {24 log(2)}
%%   =               s0         + 0.0601 * log(lam)
  \code{spar} is \eqn{s = s0 + 0.0601 * \bold{\log}\lambda}{%
                   spar = s0 + 0.0601 * log(lambda)},
  which is intentionally \emph{different} from the S-plus implementation
  of \code{smooth.spline} (where \code{spar} is proportional to
  \eqn{\lambda}).  In \R's (\eqn{\log \lambda}) scale, it makes more
  sense to vary \code{spar} linearly.

  Note however that currently the results may become very unreliable
  for \code{spar} values smaller than about -1 or -2.  The same may
  happen for values larger than 2 or so. Don't think of setting
  \code{spar} or the controls \code{low} and \code{high} outside such a
  safe range, unless you know what you are doing!

  The \dQuote{generalized} cross-validation method will work correctly when
  there are duplicated points in \code{x}.  However, it is ambiguous what
  leave-one-out cross-validation means with duplicated points, and the
  internal code uses an approximation that involves leaving out groups
  of duplicated points.  \code{cv=TRUE} is best avoided in that case.
}
\note{
  The default \code{all.knots = FALSE} and \code{nknots = NULL} entails
  using only \eqn{O(n^{0.2})}
  knots instead of \eqn{n} for \eqn{n > 49}.  This cuts speed and memory
  requirements, but not drastically anymore since \R version 1.5.1 where
  it is only \eqn{O(n_k) + O(n)}{O(nk) + O(n)} where \eqn{n_k}{nk} is
  the number of knots.
  In this case where not all unique \code{x} values are
  used as knots, the result is not a smoothing spline in the strict
  sense, but very close unless a small smoothing parameter (or large
  \code{df}) is used.
}
\value{
  An object of class \code{"smooth.spline"} with components
  \item{x}{the \emph{distinct} \code{x} values in increasing order, see
  the \bold{Details} above.}
  \item{y}{the fitted values corresponding to \code{x}.}
  \item{w}{the weights used at the unique values of \code{x}.}
  \item{yin}{the y values used at the unique \code{y} values.}
  \item{data}{only if \code{keep.data = TRUE}: itself a
    \code{\link{list}} with components \code{x}, \code{y} and \code{w}
    of the same length.  These are the original \eqn{(x_i,y_i,w_i),
      i=1,\dots,n}, values where \code{data$x} may have repeated values and
    hence be longer than the above \code{x} component; see details.
    }
  \item{lev}{leverages, the diagonal values of the smoother matrix.}
  \item{cv.crit}{cross-validation score, \dQuote{generalized} or true, depending
    on \code{cv}.}
  \item{pen.crit}{penalized criterion}
  \item{crit}{the criterion value minimized in the underlying
    \code{.Fortran} routine \file{sslvrg}.}
  \item{df}{equivalent degrees of freedom used.  Note that (currently)
    this value may become quite unprecise when the true \code{df} is
    between and 1 and 2.
  }
  \item{spar}{the value of \code{spar} computed or given.}
  \item{lambda}{the value of \eqn{\lambda} corresponding to \code{spar},
    see the details above.}
  \item{iparms}{named integer(3) vector where \code{..$ipars["iter"]}
    gives number of spar computing iterations used.}
  \item{fit}{list for use by \code{\link{predict.smooth.spline}}, with
    components
    \describe{
      \item{knot:}{the knot sequence (including the repeated boundary
	knots).}
      \item{nk:}{number of coefficients or number of \dQuote{proper}
	knots plus 2.}
      \item{coef:}{coefficients for the spline basis used.}
      \item{min, range:}{numbers giving the corresponding quantities of
	\code{x}.}
    }
  }
  \item{call}{the matched call.}
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}, Wadsworth \& Brooks/Cole.

  Green, P. J. and Silverman, B. W. (1994)
  \emph{Nonparametric Regression and Generalized Linear Models:
    A Roughness Penalty Approach.} Chapman and Hall.

  Hastie, T. J. and Tibshirani, R. J. (1990)
  \emph{Generalized Additive Models.}  Chapman and Hall.
}
\author{
  \R implementation by B. D. Ripley and Martin Maechler
  (\code{spar/lambda}, etc).

  This function is based on code in the \code{GAMFIT} Fortran program by
  T. Hastie and R. Tibshirani (\url{http://lib.stat.cmu.edu/general/}),
  which makes use of spline code by Finbarr O'Sullivan.  Its design
  parallels the \code{smooth.spline} function of Chambers \& Hastie (1992).
}
\seealso{
  \code{\link{predict.smooth.spline}} for evaluating the spline
  and its derivatives.
}
\examples{
attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)
## This example has duplicate points, so avoid cv=TRUE
\dontshow{
  stopifnot(cars.spl $ w == table(speed)) # weights = multiplicities
  str(cars.spl, digits=5, vec.len=6)
  cars.spl$fit
}
lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df=10), lty=2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg='bisque')
detach()


## Residual (Tukey Anscombe) plot:
plot(residuals(cars.spl) ~ fitted(cars.spl))
abline(h = 0, col="gray")

## consistency check:
stopifnot(all.equal(cars$dist,
                    fitted(cars.spl) + residuals(cars.spl)))

##-- artificial example
y18 <- c(1:3,5,4,7:3,2*(2:5),rep(10,4))
xx  <- seq(1,length(y18), len=201)
(s2  <- smooth.spline(y18)) # GCV
(s02 <- smooth.spline(y18, spar = 0.2))
plot(y18, main=deparse(s2$call), col.main=2)
lines(s2, col = "gray"); lines(predict(s2, xx), col = 2)
lines(predict(s02, xx), col = 3); mtext(deparse(s02$call), col = 3)

## The following shows the problematic behavior of 'spar' searching:
(s2  <- smooth.spline(y18,          con=list(trace=TRUE,tol=1e-6, low= -1.5)))
(s2m <- smooth.spline(y18, cv=TRUE, con=list(trace=TRUE,tol=1e-6, low= -1.5)))
## both above do quite similarly (Df = 8.5 +- 0.2)

\dontshow{
% not for the user
  s2. <- smooth.spline(y18, cv=TRUE,con=list(trace=TRUE, tol=1e-6,low= -3,maxit=20))
  s2. ## Intel-Linux: Df ~= (even! > ) 18 : interpolating -- much smaller PRESS
     ## {others, e.g., may end quite differently!}
  lines(predict(s2., xx), col = 4)
  mtext(deparse(s2.$call,200), side= 1, line= -1, cex= 0.8, col= 4)

  sdf8 <- smooth.spline(y18, df = 8, con=list(trace=TRUE))
  sdf8 ; sdf8$df - 8

  try(smooth.spline(y18, spar = 50)) #>> error : spar 'way too large'
}
}
\keyword{smooth}
