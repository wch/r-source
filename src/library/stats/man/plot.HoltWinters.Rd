% File src/library/stats/man/plot.HoltWinters.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

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
        col.intervals = 4, col.separator = 1, lty = 1,
        lty.predicted = 1, lty.intervals = 1, lty.separator = 3,
        ylab = "Observed / Fitted",
        main = "Holt-Winters filtering",
        ylim = NULL, \dots)
}
\arguments{
  \item{x}{Object of class \code{"HoltWinters"}}
  \item{predicted.values}{Predicted values as returned by \code{predict.HoltWinters}}
  \item{intervals}{If \code{TRUE}, the prediction intervals are plotted (default).}
  \item{separator}{If \code{TRUE}, a separating line between fitted and predicted values is plotted (default).}
  \item{col, lty}{Color/line type of original data (default: black solid).}
  \item{col.predicted, lty.predicted}{Color/line type of
    fitted and predicted values (default: red solid).}
  \item{col.intervals, lty.intervals}{Color/line type of prediction
    intervals (default: blue solid).}
  \item{col.separator, lty.separator}{Color/line type of
    observed/predicted values separator (default: black dashed).}
  \item{ylab}{Label of the y-axis.}
  \item{main}{Main title.}
  \item{ylim}{Limits of the y-axis. If \code{NULL}, the range is chosen
    such that the plot contains the original series, the fitted values,
    and the predicted values if any.}
  \item{\dots}{Other graphics parameters.}
}
\references{
  C. C. Holt (1957)
  Forecasting trends and seasonals by exponentially weighted
  moving averages,
  \emph{ONR Research Memorandum, Carnegie Institute of Technology} \bold{52}.

  P. R. Winters (1960)
  Forecasting sales by exponentially weighted moving averages,
  \emph{Management Science} \bold{6}, 324--342.
}
\author{
  David Meyer \email{David.Meyer@wu.ac.at}
}
\seealso{
  \code{\link{HoltWinters}}, \code{\link{predict.HoltWinters}}
}

\keyword{ts}
