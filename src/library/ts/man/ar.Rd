\name{ar}
\alias{ar}
\alias{ar.burg}
\alias{ar.yw}
\alias{print.ar}
\alias{predict.ar}
\title{Fit Autoregressive Models to Time Series}
\usage{
ar(x, aic=TRUE, order.max=NULL, method=c("yule-walker", "burg"),
   na.action, series)
ar.burg(x, aic=TRUE, order.max=NULL, na.action, series)
ar.yw(x, aic=TRUE, order.max=NULL, na.action, series)
predict(ar.obj, newdata, n.ahead = 1, se.fit = TRUE) 
}
\arguments{
 \item{x}{A univariate or multivariate time series}
 \item{aic}{Logical flag. If \code{TRUE} then the Akaike Information
     Criterion is used to choose the order of the autoregressive
     model. If \code{FALSE}, the model of order \code{order.max} is fitted.}
 \item{order.max}{Maximum order (or order) of model to fit.
     Defaults to 10*log10(N) where N is the number of observations.}
 \item{method}{Character string giving the method used to fit the model.
     Must be one of \code{"yule-walker"} or \code{"burg"} (the first few
     characters are sufficient). Defaults to \code{"yule-walker"}.}
 \item{na.action}{function called to remove missing values.}
 \item{series}{name for the series. Defaults to
\code{deparse(substitute(x))}.}
 \item{ar.obj}{a fit from \code{ar}.}
 \item{newdata}{data to which to apply the prediction.}
 \item{n.ahead}{number of steps ahead at which to predict.}
 \item{se.fit}{logical: return estimated standard errors of the
     prediction error?.}
}
\description{
    Fit an autoregressive time series model to the data, by default
    selecting the complexity by AIC.
}
\details{
\code{ar} is just a wrapper for the functions \code{ar.yw} and
\code{ar.burg}.

Order selection is done by AIC if \code{aic} is true. This is
problematic, as the methods here do not perform true maximum likelihood
estimation. In \code{ar.yw} the variance matrix of the innovation is
computed from the fitted coefficients and the autocovariance of
\code{x}, and the AIC is computed as if the estimate were the MLE,
omitting the determinant term from the likelihood.

Remember that \code{ar} removes the overall mean of \code{x} before
fitting the AR model.
}
\value{
  A list of class \code{"ar"} with the following elements:
  \item{order}{The order of the fitted model.  This is chosen by
     minimizing the AIC if \code{aic=TRUE}, otherise it is \code{order.max}.}
  \item{ar}{Estimated autoregression coefficients for the fitted model.}
  \item{var.pred}{The prediction variance: an estimate of the portion of the
     variance of the time series that is not explained by the
     autoregressive model.}
  \item{aic}{The value of the \code{aic} argument.}
  \item{n.used}{The number of observations in the time series.}
  \item{order.max}{The value of the \code{order.max} argument.}
  \item{partialacf}{The estimate of the partial autocorrelation function
     up to lag \code{order.max}.}
  \item{resid}{residuals from the fitted model, conditioning on the
     first \code{order} observations. The first
     \code{order} residuals are set to \code{NA}. If \code{x} is a time
     series, so is \code{resid}.}
  \item{method}{The value of the \code{method} argument.}
  \item{series}{The name(s) of the time series.}
  \item{asy.var.coef}{The asymptotic-theory variance matrix of the
      coefficient estimates (only for univariate time series).}

  For \code{predict.ar}, a time series of predictions, or if
  \code{se.fit=TRUE}, a list with components \code{pred}, the
  predictions, and \code{se}, the estimated standard errors. Both
  components are time series.
}
\author{Martyn Plummer, univariate case of \code{ar.yw} by B.D. Ripley.}

\references{
    Brockwell, P. J. and Davis, R. A. (1991) \emph{Time
	Series and Forecasting Methods.}  Second edition. Springer, New
    York. Section 11.4.

    Brockwell, P. J. and Davis, R. A. (1996) \emph{Introduction to Time
	Series and Forecasting.} Springer, New York. Sections 5.1 and 7.6.
    
    Whittle, P. (1963) On the fitting of multivariate autoregressions
    and the approximate canonical factorization of a spectral density
    matrix. \emph{Biometrika} \bold{40}, 129-134.
    
}

\note{\code{ar.burg} is not yet implemented.}

\examples{
data(lh)
ar(lh)

data(sunspot)
sunspot.ar <- ar(sunspot.year)
sunspot.ar
predict(sunspot.ar, n.ahead=25)

data(sales)
ar(ts.union(sales, sales.lead))
}
\keyword{ts}
