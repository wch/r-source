% File src/library/stats/man/predict.HoltWinters.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{predict.HoltWinters}
\alias{predict.HoltWinters}
\title{Prediction Function for Fitted Holt-Winters Models}
\description{
  Computes predictions and prediction intervals for models fitted by
  the Holt-Winters method.
}
\usage{
\method{predict}{HoltWinters}(object, n.ahead = 1, prediction.interval = FALSE,
       level = 0.95, ...)
}
\arguments{
  \item{object}{An object of class \code{HoltWinters}.}
  \item{n.ahead}{Number of future periods to predict.}
  \item{prediction.interval}{logical. If \code{TRUE}, the lower and
    upper bounds of the corresponding prediction intervals are computed.}
  \item{level}{Confidence level for the prediction interval.}
  \item{\dots}{arguments passed to or from other methods.}
}
\value{
  A time series of the predicted values. If prediction intervals are
  requested, a multiple time series is returned with columns \code{fit},
  \code{lwr} and \code{upr} for the predicted values and the lower and
  upper bounds respectively.
}
\references{
  C. C. Holt (1957)
  Forecasting trends and seasonals by exponentially weighted
  moving averages,
  \emph{ONR Research Memorandum, Carnegie Institute of Technology} \bold{52}.

  P. R. Winters (1960).
  Forecasting sales by exponentially weighted moving averages.
  \emph{Management Science}, \bold{6}, 324--342.
  \doi{10.1287/mnsc.6.3.324}.
}
\author{
  David Meyer \email{David.Meyer@wu.ac.at}
}
\seealso{\code{\link{HoltWinters}}}

\examples{
require(graphics)

m <- HoltWinters(co2)
p <- predict(m, 50, prediction.interval = TRUE)
plot(m, p)
}
\keyword{ts}


