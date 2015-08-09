% File src/library/stats/man/predict.nls.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{predict.nls}
\title{Predicting from Nonlinear Least Squares Fits}
\alias{predict.nls}
\usage{
\method{predict}{nls}(object, newdata , se.fit = FALSE, scale = NULL, df = Inf,
        interval = c("none", "confidence", "prediction"),
        level = 0.95, \dots)
}
\description{
  \code{predict.nls} produces predicted values, obtained by evaluating
  the regression function in the frame \code{newdata}.  If the logical
  \code{se.fit} is \code{TRUE}, standard errors of the predictions are
  calculated.  If the numeric argument \code{scale} is set (with
  optional \code{df}), it is used as the residual standard deviation in
  the computation of the standard errors, otherwise this is extracted
  from the model fit.  Setting \code{intervals} specifies computation of
  confidence or prediction (tolerance) intervals at the specified
  \code{level}.

  At present \code{se.fit} and \code{interval} are ignored.
}
\arguments{
 \item{object}{An object that inherits from class \code{nls}.}
 \item{newdata}{A named list or data frame in which to look for variables with
   which to predict.  If \code{newdata} is
   missing the fitted values at the original data points are returned.}
 \item{se.fit}{A logical value indicating if the standard errors of the
   predictions should be calculated.  Defaults to \code{FALSE}.  At
   present this argument is ignored.}
 \item{scale}{A numeric scalar.  If it is set (with optional
   \code{df}), it is used as the residual standard deviation in the
   computation of the standard errors, otherwise this information is
   extracted from the model fit. At present this argument is ignored.}
 \item{df}{A positive numeric scalar giving the number of degrees of
   freedom for the \code{scale} estimate. At present this argument is
   ignored.}
 \item{interval}{A character string indicating if prediction intervals
   or a confidence interval on the mean responses are to be
   calculated. At present this argument is ignored.}
 \item{level}{A numeric scalar between 0 and 1 giving the confidence
   level for the intervals (if any) to be calculated.  At present
   this argument is ignored.}
 \item{\dots}{Additional optional arguments.  At present no optional
   arguments are used.}
}
\value{
  \code{predict.nls} produces a vector of predictions.
  When implemented, \code{interval} will produce a matrix of
  predictions and bounds with column names \code{fit}, \code{lwr}, and
  \code{upr}.  When implemented, if \code{se.fit} is
  \code{TRUE}, a list with the following components will be returned:

  \item{fit}{vector or matrix as above}
  \item{se.fit}{standard error of predictions}
  \item{residual.scale}{residual standard deviations}
  \item{df}{degrees of freedom for residual}
}
\seealso{
  The model fitting function \code{\link{nls}},
  \code{\link{predict}}.
}
\note{
  Variables are first looked for in \code{newdata} and then searched for
  in the usual way (which will include the environment of the formula
  used in the fit).  A warning will be given if the
  variables found are not of the same length as those in \code{newdata}
  if it was supplied.
}
\examples{
\testonly{od <- options(digits = 5)}
require(graphics)

fm <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
predict(fm)              # fitted values at observed times
## Form data plot and smooth line for the predictions
opar <- par(las = 1)
plot(demand ~ Time, data = BOD, col = 4,
     main = "BOD data and fitted first-order curve",
     xlim = c(0,7), ylim = c(0, 20) )
tt <- seq(0, 8, length = 101)
lines(tt, predict(fm, list(Time = tt)))
par(opar)
\testonly{options(od)}
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
