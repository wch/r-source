% File src/library/stats/man/predict.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{predict}
\title{Model Predictions}
\usage{
predict (object, \dots)
}
\alias{predict}
\arguments{
  \item{object}{a model object for which prediction is desired.}
  \item{\dots}{additional arguments affecting the predictions produced.}
}
\description{
  \code{predict} is a generic function for predictions from the results of
  various model fitting functions.  The function invokes particular
  \emph{methods} which depend on the \code{\link{class}} of
  the first argument.
}
\details{
  Most prediction methods which are similar to those for linear models
  have an argument \code{newdata} specifying the first place to look for
  explanatory variables to be used for prediction.  Some considerable
  attempts are made to match up the columns in \code{newdata} to those
  used for fitting, for example that they are of comparable types and
  that any factors have the same level set in the same order (or can be
  transformed to be so).

  Time series prediction methods in package \pkg{stats} have an argument
  \code{n.ahead} specifying how many time steps ahead to predict.

  Many methods have a logical argument \code{se.fit} saying if standard
  errors are to returned.
}
 \value{
  The form of the value returned by \code{predict} depends on the
  class of its argument.  See the documentation of the
  particular methods for details of what is produced by that method.
}
\seealso{
  \code{\link{predict.glm}},
  \code{\link{predict.lm}},
  \code{\link{predict.loess}},
  \code{\link{predict.nls}},
  \code{\link{predict.poly}},
  \code{\link{predict.princomp}},
  \code{\link{predict.smooth.spline}}.

  \link{SafePrediction} for prediction from (univariable) polynomial and
  spline fits.

  For time-series prediction,
  \code{\link{predict.ar}},
  \code{\link{predict.Arima}},
  \code{\link{predict.arima0}},
  \code{\link{predict.HoltWinters}},
  \code{\link{predict.StructTS}}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.
}
\examples{
\dontshow{old <- Sys.setlocale("LC_COLLATE", "C")}
require(utils)

## All the "predict" methods found
## NB most of the methods in the standard packages are hidden.
## Output will depend on what namespaces are (or have been) loaded.
## IGNORE_RDIFF_BEGIN
for(fn in methods("predict"))
   try({
       f <- eval(substitute(getAnywhere(fn)$objs[[1]], list(fn = fn)))
       cat(fn, ":\n\t", deparse(args(f)), "\n")
       }, silent = TRUE)
## IGNORE_RDIFF_END
\dontshow{old <- Sys.setlocale("LC_COLLATE", old)}
}
\keyword{methods}
