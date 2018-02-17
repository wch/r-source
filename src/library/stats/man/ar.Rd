% File src/library/stats/man/ar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{ar}
\title{Fit Autoregressive Models to Time Series}
\alias{ar}
\alias{ar.burg}
\alias{ar.burg.default}
\alias{ar.burg.mts}
\alias{ar.yw}
\alias{ar.yw.default}
\alias{ar.yw.mts}
\alias{ar.mle}
\alias{print.ar}
\alias{predict.ar}
\concept{autoregression}
\usage{
ar(x, aic = TRUE, order.max = NULL,
   method = c("yule-walker", "burg", "ols", "mle", "yw"),
   na.action, series, \dots)

ar.burg(x, \dots)
\method{ar.burg}{default}(x, aic = TRUE, order.max = NULL,
        na.action = na.fail, demean = TRUE, series,
        var.method = 1, \dots)
\method{ar.burg}{mts}(x, aic = TRUE, order.max = NULL,
        na.action = na.fail, demean = TRUE, series,
        var.method = 1, \dots)

ar.yw(x, \dots)
\method{ar.yw}{default}(x, aic = TRUE, order.max = NULL,
      na.action = na.fail, demean = TRUE, series, \dots)
\method{ar.yw}{mts}(x, aic = TRUE, order.max = NULL,
      na.action = na.fail, demean = TRUE, series,
      var.method = 1, \dots)

ar.mle(x, aic = TRUE, order.max = NULL, na.action = na.fail,
       demean = TRUE, series, \dots)

\method{predict}{ar}(object, newdata, n.ahead = 1, se.fit = TRUE, \dots)
}
\arguments{
  \item{x}{a univariate or multivariate time series.}

  \item{aic}{\code{\link{logical}}.  If \code{TRUE} then the Akaike Information
    Criterion is used to choose the order of the autoregressive
    model.  If \code{FALSE}, the model of order \code{order.max} is
    fitted.}

  \item{order.max}{maximum order (or order) of model to fit.  Defaults
    to the smaller of \eqn{N-1} and \eqn{10\log_{10}(N)}{10*log10(N)}
    where \eqn{N} is the number of non-missing observations
    except for \code{method = "mle"} where it is the minimum of this
    quantity and 12.}

  \item{method}{character string specifying the method to fit the
    model.  Must be one of the strings in the default argument
    (the first few characters are sufficient).  Defaults to
    \code{"yule-walker"}.}

  \item{na.action}{function to be called to handle missing
    values.  Currently, via \code{na.action = na.pass}, only Yule-Walker
    method can handle missing values which must be consistent within a
    time point: either all variables must be missing or none.}

  \item{demean}{should a mean be estimated during fitting?}

  \item{series}{names for the series.  Defaults to
    \code{deparse(substitute(x))}.}

  \item{var.method}{the method to estimate the innovations variance
    (see \sQuote{Details}).}

  \item{\dots}{additional arguments for specific methods.}

  \item{object}{a fit from \code{ar()}.}

  \item{newdata}{data to which to apply the prediction.}

  \item{n.ahead}{number of steps ahead at which to predict.}

  \item{se.fit}{logical: return estimated standard errors of the
    prediction error?}
}
\description{
  Fit an autoregressive time series model to the data, by default
  selecting the complexity by AIC.
}
\details{
  For definiteness, note that the AR coefficients have the sign in

  \deqn{x_t - \mu = a_1(x_{t-1} - \mu) + \cdots +  a_p(x_{t-p} - \mu) + e_t}{x[t] - m = a[1]*(x[t-1] - m) + \dots +  a[p]*(x[t-p] - m) + e[t]}

  \code{ar} is just a wrapper for the functions \code{ar.yw},
  \code{ar.burg}, \code{\link{ar.ols}} and \code{ar.mle}.

  Order selection is done by AIC if \code{aic} is true. This is
  problematic, as of the methods here only \code{ar.mle} performs
  true maximum likelihood estimation. The AIC is computed as if the variance
  estimate were the MLE, omitting the determinant term from the
  likelihood. Note that this is not the same as the Gaussian likelihood
  evaluated at the estimated parameter values.  In \code{ar.yw} the
  variance matrix of the innovations is computed from the fitted
  coefficients and the autocovariance of \code{x}.

  \code{ar.burg} allows two methods to estimate the innovations
  variance and hence AIC. Method 1 is to use the update given by
  the Levinson-Durbin recursion (Brockwell and Davis, 1991, (8.2.6)
  on page 242), and follows S-PLUS. Method 2 is the mean of the sum
  of squares of the forward and backward prediction errors
  (as in Brockwell and Davis, 1996, page 145). Percival and Walden
  (1998) discuss both. In the multivariate case the estimated
  coefficients will depend (slightly) on the variance estimation method.

  Remember that \code{ar} includes by default a constant in the model, by
  removing the overall mean of \code{x} before fitting the AR model,
  or (\code{ar.mle}) estimating a constant to subtract.
}
\value{
  For \code{ar} and its methods a list of class \code{"ar"} with
  the following elements:
  \item{order}{The order of the fitted model.  This is chosen by
    minimizing the AIC if \code{aic = TRUE}, otherwise it is \code{order.max}.}
  \item{ar}{Estimated autoregression coefficients for the fitted model.}
  \item{var.pred}{The prediction variance: an estimate of the portion of the
    variance of the time series that is not explained by the
    autoregressive model.}
  \item{x.mean}{The estimated mean of the series used in fitting and for
    use in prediction.}
  \item{x.intercept}{(\code{ar.ols} only.) The intercept in the model for
    \code{x - x.mean}.}
  \item{aic}{The differences in AIC between each model and the
    best-fitting model.  Note that the latter can have an AIC of \code{-Inf}.}
  \item{n.used}{The number of observations in the time series, including
    missing.}
  \item{n.obs}{The number of non-missing observations in the time series.}
  \item{order.max}{The value of the \code{order.max} argument.}
  \item{partialacf}{The estimate of the partial autocorrelation function
    up to lag \code{order.max}.}
  \item{resid}{residuals from the fitted model, conditioning on the
    first \code{order} observations. The first \code{order} residuals
    are set to \code{NA}. If \code{x} is a time series, so is \code{resid}.}
  \item{method}{The value of the \code{method} argument.}
  \item{series}{The name(s) of the time series.}
  \item{frequency}{The frequency of the time series.}
  \item{call}{The matched call.}
  \item{asy.var.coef}{(univariate case, \code{order > 0}.)
    The asymptotic-theory variance matrix of the coefficient estimates.}

  For \code{predict.ar}, a time series of predictions, or if
  \code{se.fit = TRUE}, a list with components \code{pred}, the
  predictions, and \code{se}, the estimated standard errors.  Both
  components are time series.
}
\author{
  Martyn Plummer. Univariate case of \code{ar.yw}, \code{ar.mle}
  and C code for univariate case of \code{ar.burg} by B. D. Ripley.
}

\note{
  Only the univariate case of \code{ar.mle} is implemented.

  Fitting by \code{method="mle"} to long series can be very slow.

  If \code{x} contains missing values, see \code{\link{NA}}, also consider
  using \code{\link{arima}()}, possibly with \code{method = "ML"}.
}

\seealso{
  \code{\link{ar.ols}}, \code{\link{arima}} for ARMA models;
  \code{\link{acf2AR}}, for AR construction from the ACF.

  \code{\link{arima.sim}} for simulation of AR processes.
}

\references{
  Brockwell, P. J. and Davis, R. A. (1991).
  \emph{Time Series and Forecasting Methods}, second edition.
  Springer, New York.
  Section 11.4.

  Brockwell, P. J. and Davis, R. A. (1996).
  \emph{Introduction to Time Series and Forecasting}.
  Springer, New York.
  Sections 5.1 and 7.6.

  Percival, D. P. and Walden, A. T. (1998).
  \emph{Spectral Analysis for Physical Applications}.
  Cambridge University Press.

  Whittle, P. (1963).
  On the fitting of multivariate autoregressions and the approximate
  canonical factorization of a spectral density matrix.
  \emph{Biometrika}, \bold{40}, 129--134.
  \doi{10.2307/2333753}.
}

\examples{
ar(lh)
ar(lh, method = "burg")
ar(lh, method = "ols")
ar(lh, FALSE, 4) # fit ar(4)

(sunspot.ar <- ar(sunspot.year))
predict(sunspot.ar, n.ahead = 25)
## try the other methods too

ar(ts.union(BJsales, BJsales.lead))
## Burg is quite different here, as is OLS (see ar.ols)
ar(ts.union(BJsales, BJsales.lead), method = "burg")
}
\keyword{ts}
