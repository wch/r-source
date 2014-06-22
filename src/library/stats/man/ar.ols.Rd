% File src/library/stats/man/ar.ols.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{ar.ols}
\alias{ar.ols}
\title{Fit Autoregressive Models to Time Series by OLS}
\usage{
ar.ols(x, aic = TRUE, order.max = NULL, na.action = na.fail,
       demean = TRUE, intercept = demean, series, \dots)
}
\arguments{
  \item{x}{A univariate or multivariate time series.}

  \item{aic}{Logical flag.  If \code{TRUE} then the Akaike Information
    Criterion is used to choose the order of the autoregressive
    model. If \code{FALSE}, the model of order \code{order.max} is
    fitted.}

  \item{order.max}{Maximum order (or order) of model to fit. Defaults
    to \eqn{10\log_{10}(N)}{10*log10(N)} where \eqn{N} is the number
    of observations.}

  \item{na.action}{function to be called to handle missing values.}

  \item{demean}{should the AR model be for \code{x} minus its mean?}

  \item{intercept}{should a separate intercept term be fitted?}

  \item{series}{names for the series.  Defaults to
    \code{deparse(substitute(x))}.}

  \item{\dots}{further arguments to be passed to or from methods.}
}
\description{
  Fit an autoregressive time series model to the data by ordinary
  least squares, by default selecting the complexity by AIC.
}
\details{
  \code{ar.ols} fits the general AR model to a possibly non-stationary
  and/or multivariate system of series \code{x}. The resulting
  unconstrained least squares estimates are consistent, even if
  some of the series are non-stationary and/or co-integrated.
  For definiteness, note that the AR coefficients have the sign in

  \deqn{x_t - \mu = a_0 + a_1(x_{t-1} - \mu) + \cdots + a_p(x_{t-p} - \mu) + e_t}{(x[t] - m) = a[0] + a[1]*(x[t-1] - m) + \dots +  a[p]*(x[t-p] - m) + e[t]}

  where \eqn{a_0}{a[0]} is zero unless \code{intercept} is true, and
  \eqn{\mu}{m} is the sample mean if \code{demean} is true, zero
  otherwise.

  Order selection is done by AIC if \code{aic} is true. This is
  problematic, as \code{ar.ols} does not perform
  true maximum likelihood estimation. The AIC is computed as if
  the variance estimate (computed from the variance matrix of the
  residuals) were the MLE, omitting the determinant term from the
  likelihood. Note that this is not the same as the Gaussian
  likelihood evaluated at the estimated parameter values.

  Some care is needed if \code{intercept} is true and \code{demean} is
  false. Only use this is the series are roughly centred on
  zero. Otherwise the computations may be inaccurate or fail entirely.
}
\value{
  A list of class \code{"ar"} with the following elements:
  \item{order}{The order of the fitted model.  This is chosen by
    minimizing the AIC if \code{aic = TRUE}, otherwise it is
    \code{order.max}.}
  \item{ar}{Estimated autoregression coefficients for the fitted
    model.}
  \item{var.pred}{The prediction variance: an estimate of the portion of
    the variance of the time series that is not explained by the
    autoregressive model.}
  \item{x.mean}{The estimated mean (or zero if \code{demean} is false)
    of the series used in fitting and for use in prediction.}
  \item{x.intercept}{The intercept in the model for
    \code{x - x.mean}, or zero if \code{intercept} is false.}
  \item{aic}{The differences in AIC between each model and the
    best-fitting model.  Note that the latter can have an AIC of \code{-Inf}.}
  \item{n.used}{The number of observations in the time series.}
  \item{order.max}{The value of the \code{order.max} argument.}
  \item{partialacf}{\code{NULL}.  For compatibility with \code{ar}.}
  \item{resid}{residuals from the fitted model, conditioning on the
    first \code{order} observations.  The first \code{order} residuals
    are set to \code{NA}. If \code{x} is a time series, so is
    \code{resid}.}
  \item{method}{The character string \code{"Unconstrained LS"}.}
  \item{series}{The name(s) of the time series.}
  \item{frequency}{The frequency of the time series.}
  \item{call}{The matched call.}
  \item{asy.se.coef}{The asymptotic-theory standard errors of the
    coefficient estimates.}
}
\author{Adrian Trapletti, Brian Ripley.}
\seealso{
  \code{\link{ar}}
}
\references{
  Luetkepohl, H. (1991): \emph{Introduction to Multiple Time Series
    Analysis.} Springer Verlag, NY, pp.\sspace{}368--370.
}
\examples{
ar(lh, method = "burg")
ar.ols(lh)
ar.ols(lh, FALSE, 4) # fit ar(4)

ar.ols(ts.union(BJsales, BJsales.lead))

x <- diff(log(EuStockMarkets))
ar.ols(x, order.max = 6, demean = FALSE, intercept = TRUE)
}
\keyword{ts}
