% file lqs/man/lqs.Rd
% copyright (C) 1998-9 B. D. Ripley    (C) 2001-2 R Core Development Team
%
\name{lqs}
\alias{lqs}
\alias{lqs.formula}
\alias{lqs.default}
\alias{lmsreg}
\alias{ltsreg}
%\alias{print.lqs}
\title{
Resistant Regression
}
\description{
  Fit a regression to the \emph{good} points in the dataset, thereby
  achieving a regression estimator with a high breakdown point.
  \code{lmsreg} and \code{ltsreg} are compatibility wrappers.
}
\usage{
lqs(x, \dots)

\method{lqs}{formula}(formula, data, \dots,
    method = c("lts", "lqs", "lms", "S", "model.frame"),
    subset, na.action, model = TRUE,
    x.ret = FALSE, y.ret = FALSE, contrasts = NULL)

\method{lqs}{default}(x, y, intercept = TRUE, method = c("lts", "lqs", "lms", "S"),
    quantile, control = lqs.control(\dots), k0 = 1.548, seed, \dots)

lmsreg(\dots)
ltsreg(\dots)
}
\arguments{
  \item{formula}{a formula of the form \code{y ~ x1 + x2 + \dots}.}
  \item{data}{data frame from which variables specified in
    \code{formula} are preferentially to be taken.}
  \item{subset}{an index vector specifying the cases to be used in
    fitting. (NOTE: If given, this argument must be named exactly.)}
  \item{na.action}{function to specify the action to be taken if
    \code{NA}s are found.  The default action is for the procedure to
    fail.  Alternatives include \code{\link{na.omit}} and
    \code{\link{na.exclude}}, which lead to omission of
    cases with missing values on any required variable.  (NOTE: If
    given, this argument must be named exactly.)
  }
  \item{model, x.ret, y.ret}{logical. If \code{TRUE} the model frame,
    the model matrix and the response are returned, respectively.}
  \item{contrasts}{an optional list.  See the \code{contrasts.arg}
    of \code{\link{model.matrix.default}}.}
  \item{x}{a matrix or data frame containing the explanatory variables.}
  \item{y}{the response: a vector of length the number of rows of \code{x}.}
  \item{intercept}{should the model include an intercept?}
  \item{method}{
    the method to be used. \code{model.frame} returns the model frame: for the
    others see the \code{Details} section. Using \code{lmsreg} or
    \code{ltsreg} forces \code{"lms"} and \code{"lts"} respectively.
  }
  \item{quantile}{
    the quantile to be used: see \code{Details}. This is over-ridden if
    \code{method = "lms"}.
  }
  \item{control}{additional control items: see \code{Details}.}
  \item{k0}{the cutoff / tuning constant used for \eqn{\chi()}{chi()}
    and \eqn{\psi()}{psi()} functions when \code{method = "S"}, currently
    corresponding to Tukey's \dQuote{biweight}.}
  \item{seed}{
    the seed to be used for random sampling: see \code{.Random.seed}. The
    current value of \code{.Random.seed} will be preserved if it is set..
  }
  \item{\dots}{arguments to be passed to \code{lqs.default} or
    \code{lqs.control}, see \code{control} above and \code{Details}.}
}
\value{
  An object of class \code{"lqs"}.  This is a list with components
  \item{crit}{the value of the criterion for the best solution found, in
    the case of \code{method == "S"} before IWLS refinement.}
  \item{sing}{character. A message about the number of samples which
    resulted in singular fits.}
  \item{coefficients}{of the fitted linear model}
  \item{bestone}{the indices of those points fitted by the best sample
    found (prior to adjustment of the intercept, if requested).}
  \item{fitted.values}{the fitted values.}
  \item{residuals}{the residuals.}
  \item{scale}{estimate(s) of the scale of the error. The first is based
    on the fit criterion.  The second (not present for \code{method ==
      "S"}) is based on the variance of those residuals whose absolute
    value is less than 2.5 times the initial estimate.}
}
\details{
  Suppose there are \code{n} data points and \code{p} regressors,
  including any intercept.

  The first three methods minimize some function of the sorted squared
  residuals. For methods \code{"lqs"} and \code{"lms"} is the
  \code{quantile} squared residual, and for \code{"lts"} it is the sum
  of the \code{quantile} smallest squared residuals. \code{"lqs"} and
  \code{"lms"} differ in the defaults for \code{quantile}, which are
  \code{floor((n+p+1)/2)} and \code{floor((n+1)/2)} respectively.
  For \code{"lts"} the default is \code{floor(n/2) + floor((p+1)/2)}.

  The \code{"S"} estimation method solves for the scale \code{s}
  such that the average of a function chi of the residuals divided
  by \code{s} is equal to a given constant.

  The \code{control} argument is a list with components
  \describe{
    \item{\code{psamp}:}{the size of each sample. Defaults to \code{p}.}
    \item{\code{nsamp}:}{the number of samples or \code{"best"} (the
      default) or \code{"exact"} or \code{"sample"}.
      If \code{"sample"} the number chosen is \code{min(5*p, 3000)},
      taken from Rousseeuw and Hubert (1997).
      If \code{"best"} exhaustive enumeration is done up to 5000 samples;
      if \code{"exact"} exhaustive enumeration will be attempted however
      many samples are needed.}
    \item{\code{adjust}:}{should the intercept be optimized for each
      sample?  Defaults to \code{TRUE}.}
    }
}
\note{
  There seems no reason other than historical to use the \code{lms} and
  \code{lqs} options.  LMS estimation is of low efficiency (converging
  at rate \eqn{n^{-1/3}}) whereas LTS has the same asymptotic efficiency
  as an M estimator with trimming at the quartiles (Marazzi, 1993, p.201).
  LQS and LTS have the same maximal breakdown value of
  \code{(floor((n-p)/2) + 1)/n} attained if
  \code{floor((n+p)/2) <= quantile <= floor((n+p+1)/2)}.
  The only drawback mentioned of LTS is greater computation, as a sort
  was thought to be required (Marazzi, 1993, p.201) but this is not
  true as a partial sort can be used (and is used in this implementation).

  Adjusting the intercept for each trial fit does need the residuals to
  be sorted, and may be significant extra computation if \code{n} is large
  and \code{p} small.

  Opinions differ over the choice of \code{psamp}.  Rousseeuw and Hubert
  (1997) only consider p; Marazzi (1993) recommends p+1 and suggests
  that more samples are better than adjustment for a given computational
  limit.

  The computations are exact for a model with just an intercept and
  adjustment, and for LQS for a model with an intercept plus one
  regressor and exhaustive search with adjustment. For all other cases
  the minimization is only known to be approximate.
}
\author{B. D. Ripley}
\references{
  P. J. Rousseeuw and A. M. Leroy (1987)
  \emph{Robust Regression and Outlier Detection.}
  Wiley.

  A. Marazzi (1993)
  \emph{Algorithms, Routines and S Functions for Robust Statistics.}
  Wadsworth and Brooks/Cole.

  P. Rousseeuw and M. Hubert (1997) Recent developments in PROGRESS. In
  \emph{L1-Statistical Procedures and Related Topics},
  ed Y. Dodge, IMS Lecture Notes volume \bold{31}, pp. 201--214.
}
\seealso{
\code{\link{predict.lqs}}
}
\examples{
data(stackloss)
set.seed(123)
lqs(stack.loss ~ ., data = stackloss)
lqs(stack.loss ~ ., data = stackloss, method = "S", nsamp = "exact")
}
\keyword{models}
\keyword{robust}
