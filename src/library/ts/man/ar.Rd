\name{ar}
\alias{ar}
\alias{ar.burg}
\alias{ar.yw}
\title{Fit Autoregressive Models to Time Series}
\usage{
ar(x, aic=TRUE, order.max=NULL, method=c("yule-walker", "burg"),
   na.action)
ar.burg(x, aic=TRUE, order.max=NULL, na.action)
ar.yw(x, aic=TRUE, order.max=NULL, na.action)
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
\code{x}, and the AIC is computed as if the estimate were the MLE.

Remember that \code{ar} removes the overall mean of \code{x} before
fitting the AR model.
}
\value{
  A list with the following elements:
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
}
\author{Martyn Plummer, univariate case of \code{ar.yw} by B.D. Ripley.}

\note{\code{ar.burg} is not yet implemented.}

\examples{
data(lh)
ar(lh)

data(sunspot)
ar(sunspot.year)
}
\keyword{ts}
