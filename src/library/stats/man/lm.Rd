\name{lm}
\alias{lm}
\alias{print.lm}
\concept{regression}
\title{Fitting Linear Models}
\description{
  \code{lm} is used to fit linear models.
  It can be used to carry out regression,
  single stratum analysis of variance and
  analysis of covariance (although \code{\link{aov}} may provide a more
  convenient interface for these).
}
\usage{
lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, \dots)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{data}{an optional data frame containing the variables
    in the model.  If not found in \code{data}, the variables are taken
    from \code{environment(formula)}, typically the environment from which
    \code{lm} is called.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{weights}{an optional vector of weights to be used
    in the fitting process. If specified, weighted least squares is used
    with weights \code{weights} (that is, minimizing \code{sum(w*e^2)});
    otherwise ordinary least squares is used.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.}

  \item{method}{the method to be used; for fitting, currently only
    \code{method = "qr"} is supported; \code{method = "model.frame"} returns
    the model frame (the same as with \code{model = TRUE}, see below).}

  \item{model, x, y, qr}{logicals.  If \code{TRUE} the corresponding
    components of the fit (the model frame, the model matrix, the
    response, the QR decomposition) are returned. \code{qr=FALSE} is for
    S-compatibility and currently unimplemented.
  }

  \item{singular.ok}{logical. If \code{FALSE} (the default in S but
    not in \R) a singular fit is an error.}

  \item{contrasts}{an optional list. See the \code{contrasts.arg}
    of \code{model.matrix.default}.}

  \item{offset}{this can be used to specify an \emph{a priori}
    known component to be included in the linear predictor
    during fitting.  An \code{\link{offset}} term can be included in the
    formula instead or as well, and if both are specified their sum is used.}

  \item{\dots}{additional arguments to be passed to the low level
    regression fitting functions (see below).}
}
\details{
  Models for \code{lm} are specified symbolically.  A typical model has
  the form \code{response ~ terms} where \code{response} is the (numeric)
  response vector and \code{terms} is a series of terms which specifies a
  linear predictor for \code{response}.  A terms specification of the form
  \code{first + second} indicates all the terms in \code{first} together
  with all the terms in \code{second} with duplicates removed.  A
  specification of the form \code{first:second} indicates the set of
  terms obtained by taking the interactions of all terms in \code{first}
  with all terms in \code{second}.  The specification \code{first*second}
  indicates the \emph{cross} of \code{first} and \code{second}.  This is
  the same as \code{first + second + first:second}.

  If \code{response} is a matrix a linear model is fitted separately by
  least-squares to each column of the matrix.
  
  See \code{\link{model.matrix}} for some further details.  The terms in
  the formula will be re-ordered so that main effects come first,
  followed by the interactions, all second-order, all third-order and so
  on: to avoid this pass a \code{terms} object as the formula.

  A formula has an implied intercept term.  To remove this use either
  \code{y ~ x - 1} or \code{y ~ 0 + x}.  See \code{\link{formula}} for
  more details of allowed formulae.

  \code{lm} calls the lower level functions \code{\link{lm.fit}}, etc,
  see below, for the actual numerical computations.  For programming
  only, you may consider doing likewise.

  All of \code{weights}, \code{subset} and \code{offset} are evaluated
  in the same way as variables in \code{formula}, that is first in
  \code{data} and then in the environment of \code{formula}.
}
\value{
  \code{lm} returns an object of \code{\link[base]{class}} \code{"lm"} or for
  multiple responses of class \code{c("mlm", "lm")}.

  The functions \code{summary} and \code{\link{anova}} are used to
  obtain and print a summary and analysis of variance table of the
  results.  The generic accessor functions \code{coefficients},
  \code{effects}, \code{fitted.values} and \code{residuals} extract
  various useful features of the value returned by \code{lm}.

  An object of class \code{"lm"} is a list containing at least the
  following components:

  \item{coefficients}{a named vector of coefficients}
  \item{residuals}{the residuals, that is response minus fitted values.}
  \item{fitted.values}{the fitted mean values.}
  \item{rank}{the numeric rank of the fitted linear model.}
  \item{weights}{(only for weighted fits) the specified weights.}
  \item{df.residual}{the residual degrees of freedom.}
  \item{call}{the matched call.}
  \item{terms}{the \code{\link{terms}} object used.}
  \item{contrasts}{(only where relevant) the contrasts used.}
  \item{xlevels}{(only where relevant) a record of the levels of the
    factors used in fitting.}
  \item{y}{if requested, the response used.}
  \item{x}{if requested, the model matrix used.}
  \item{model}{if requested (the default), the model frame used.}

  In addition, non-null fits will have components \code{assign},
  \code{effects} and (unless not requested) \code{qr} relating to the linear
  fit, for use by extractor functions such as \code{summary} and
  \code{\link{effects}}.
}
\section{Using time series}{
  Considerable care is needed when using \code{lm} with time series.
  
  Unless \code{na.action = NULL}, the time series attributes are
  stripped from the variables before the regression is done.  (This is
  necessary as omitting \code{NA}s would invalidate the time series
  attributes, and if \code{NA}s are omitted in the middle of the series
  the result would no longer be a regular time series.)

  Even if the time series attributes are retained, they are not used to
  line up series, so that the time shift of a lagged or differenced
  regressor would be ignored.  It is good practice to prepare a
  \code{data} argument by \code{\link{ts.intersect}(\dots, dframe = TRUE)},
  then apply a suitable \code{na.action} to that data frame and call
  \code{lm} with \code{na.action = NULL} so that residuals and fitted
  values are time series.
}
\seealso{
  \code{\link{summary.lm}} for summaries and \code{\link{anova.lm}} for
  the ANOVA table; \code{\link{aov}} for a different interface.

  The generic functions \code{\link{coef}}, \code{\link{effects}},
  \code{\link{residuals}}, \code{\link{fitted}}, \code{\link{vcov}}.

  \code{\link{predict.lm}} (via \code{\link{predict}}) for prediction,
  including confidence and prediction intervals.

  \code{\link{lm.influence}} for regression diagnostics, and
  \code{\link{glm}} for \bold{generalized} linear models.

  The underlying low level functions,
  \code{\link{lm.fit}} for plain, and \code{\link{lm.wfit}} for weighted
  regression fitting.
}
\references{
  Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.

  Wilkinson, G. N. and Rogers, C. E. (1973)
  Symbolic descriptions of factorial models for analysis of variance.
  \emph{Applied Statistics}, \bold{22}, 392--9.
}
\author{
  The design was inspired by the S function of the same name described
  in Chambers (1992).  The implementation of model formula by Ross Ihaka
  was based on Wilkinson \& Rogers (1973).
}

\note{
  Offsets specified by \code{offset} will not be included in predictions
  by \code{\link{predict.lm}}, whereas those specified by an offset term
  in the formula will be.
}
\examples{
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
anova(lm.D9 <- lm(weight ~ group))
summary(lm.D90 <- lm(weight ~ group - 1))# omitting intercept
summary(resid(lm.D9) - resid(lm.D90)) #- residuals almost identical

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)

## model frame :
stopifnot(identical(lm(weight ~ group, method = "model.frame"),
                    model.frame(lm.D9)))
}
\keyword{regression}
