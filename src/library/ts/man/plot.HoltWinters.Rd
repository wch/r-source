\name{plot.HoltWinters}
\alias{plot.HoltWinters}
\title{Plot function for HoltWinters objects}
\description{
  Produces a chart of the original time series along with the fitted
  values. Optionally, predicted values (and their confidence bounds) can
  also be plotted.
}
\usage{
\method{plot}{HoltWinters}(x, predicted.values = NA, intervals = TRUE,
                 separator = TRUE, col = 1, col.predicted = 2,
                 col.intervals = 4, lty.separator = 3,
                 ylab = "Observed / Fitted",
                 main = "Holt-Winters filtering", ...)
}
\arguments{
  \item{x}{Object of class \code{"HoltWinters"}}
  \item{predicted.values}{Predicted values as returned by \code{predict.HoltWinters}}
  \item{intervals}{If \code{TRUE}, the prediction intervals are plotted (default).}
  \item{separator}{If \code{TRUE}, a separating line between fitted and predicted values is plotted (default).}
  \item{col}{Color of original data (default: black)}
  \item{col.predicted}{Color of fitted/predicted values (default: black).}
  \item{col.intervals}{Color of predicition intervals (default: blue).}
  \item{lty.separator}{Line type of the separator (default: dashed line).}
  \item{ylab}{Label of the y-axis.}
  \item{main}{Main title.}
  \item{\dots}{Other graphics parametes.}
}
\references{
  C.C Holt (1957)
  Forecasting seasonals and trends by exponentially weighted
  moving averages,
  ONR Research Memorandum, Carnigie Institute 52.
  
  P.R Winters (1960)
  Forecasting sales by exponentially weighted moving averages,
  \emph{Management Science} \bold{6}, 324--342.
}
\author{
  David Meyer \email{david.meyer@ci.tuwien.ac.at}
}
\seealso{
  \code{\link{HoltWinters}}, \code{\link{predict.HoltWinters}}
}

\keyword{ts}
