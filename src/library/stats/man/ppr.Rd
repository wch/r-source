% File src/library/stats/man/ppr.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

% file stats/man/ppr.Rd
% copyright (C) 1995-8 B. D. Ripley
% copyright (C) 2000-3   The R Core Team

\name{ppr}
\alias{ppr}
\alias{ppr.default}
\alias{ppr.formula}
\title{Projection Pursuit Regression}
\description{
  Fit a projection pursuit regression model.
}
\usage{
ppr(x, \dots)

\method{ppr}{formula}(formula, data, weights, subset, na.action,
    contrasts = NULL, \dots, model = FALSE)

\method{ppr}{default}(x, y, weights = rep(1, n),
    ww = rep(1, q), nterms, max.terms = nterms, optlevel = 2,
    sm.method = c("supsmu", "spline", "gcvspline"),
    bass = 0, span = 0, df = 5, gcvpen = 1, trace = FALSE, \dots)
}
\arguments{
  \item{formula}{
    a formula specifying one or more numeric response variables and the
    explanatory variables.
  }
  \item{x}{
    numeric matrix of explanatory variables.  Rows represent observations, and
    columns represent variables.  Missing values are not accepted.
  }
  \item{y}{
    numeric matrix of response variables.  Rows represent observations, and
    columns represent variables.  Missing values are not accepted.
  }
  \item{nterms}{number of terms to include in the final model.}
  \item{data}{
    a data frame (or similar: see \code{\link{model.frame}}) from which
    variables specified in \code{formula} are preferentially to be taken.
  }
  \item{weights}{a vector of weights \code{w_i} for each \emph{case}.}
  \item{ww}{
    a vector of weights for each \emph{response}, so the fit criterion is
    the sum over case \code{i} and responses \code{j} of
    \code{w_i ww_j (y_ij - fit_ij)^2} divided by the sum of \code{w_i}.
  }
  \item{subset}{
    an index vector specifying the cases to be used in the training
    sample.  (NOTE: If given, this argument must be named.)
  }
  \item{na.action}{
    a function to specify the action to be taken if \code{\link{NA}}s are
    found. The default action is given by \code{getOption("na.action")}.
    (NOTE: If given, this argument must be named.)
  }
  \item{contrasts}{
    the contrasts to be used when any factor explanatory variables are coded.
  }
  \item{max.terms}{
    maximum number of terms to choose from when building the model.
  }
  \item{optlevel}{
    integer from 0 to 3 which determines the thoroughness of an
    optimization routine in the SMART program. See the \sQuote{Details}
    section.
  }
  \item{sm.method}{
    the method used for smoothing the ridge functions.  The default is
    to use Friedman's super smoother \code{\link{supsmu}}.  The
    alternatives are to use the smoothing spline code underlying
    \code{\link{smooth.spline}}, either with a specified (equivalent)
    degrees of freedom for each ridge functions, or to allow the
    smoothness to be chosen by GCV.

    Can be abbreviated.
  }
  \item{bass}{
    super smoother bass tone control used with automatic span selection
    (see \code{supsmu}); the range of values is 0 to 10, with larger values
    resulting in increased smoothing.
  }
  \item{span}{
    super smoother span control (see \code{\link{supsmu}}).  The default, \code{0},
    results in automatic span selection by local cross validation. \code{span}
    can also take a value in \code{(0, 1]}.
  }
  \item{df}{
    if \code{sm.method} is \code{"spline"} specifies the smoothness of
    each ridge term via the requested equivalent degrees of freedom.
  }
  \item{gcvpen}{
    if \code{sm.method} is \code{"gcvspline"} this is the penalty used
    in the GCV selection for each degree of freedom used.
  }
  \item{trace}{logical indicating if each spline fit should produce
    diagnostic output (about \code{lambda} and \code{df}), and the
    supsmu fit about its steps.}
  \item{\dots}{arguments to be passed to or from other methods.}
  \item{model}{logical.  If true, the model frame is returned.}
}
\value{
A list with the following components, many of which are for use by the
method functions.

\item{call}{the matched call}
\item{p}{the number of explanatory variables (after any coding)}
\item{q}{the number of response variables}
\item{mu}{the argument \code{nterms}}
\item{ml}{the argument \code{max.terms}}
\item{gof}{the overall residual (weighted) sum of squares for the
  selected model}
\item{gofn}{the overall residual (weighted) sum of squares against the
  number of terms, up to \code{max.terms}.  Will be invalid (and zero)
  for less than \code{nterms}.}
\item{df}{the argument \code{df}}
\item{edf}{if \code{sm.method} is \code{"spline"} or \code{"gcvspline"}
  the equivalent number of degrees of freedom for each ridge term used.}
\item{xnames}{the names of the explanatory variables}
\item{ynames}{the names of the response variables}
\item{alpha}{a matrix of the projection directions, with a column for
  each ridge term}
\item{beta}{a matrix of the coefficients applied for each response to
  the ridge terms: the rows are the responses and the columns the ridge terms}
\item{yb}{the weighted means of each response}
\item{ys}{the overall scale factor used: internally the responses are
  divided by \code{ys} to have unit total weighted sum of squares.}
\item{fitted.values}{the fitted values, as a matrix if \code{q > 1}.}
\item{residuals}{the residuals, as a matrix if \code{q > 1}.}
\item{smod}{internal work array, which includes the ridge functions
  evaluated at the training set points.}
\item{model}{(only if \code{model = TRUE}) the model frame.}
}
\details{
  The basic method is given by Friedman (1984), and is essentially the
  same code used by S-PLUS's \code{ppreg}.  This code is extremely
  sensitive to the compiler used.

  The algorithm first adds up to \code{max.terms} ridge terms one at a
  time; it will use less if it is unable to find a term to add that makes
  sufficient difference.  It then removes the least
  important term at each step until \code{nterms} terms
  are left.

  The levels of optimization (argument \code{optlevel})
  differ in how thoroughly the models are refitted during this process.
  At level 0 the existing ridge terms are not refitted.  At level 1
  the projection directions are not refitted, but the ridge
  functions and the regression coefficients are.
%
  Levels 2 and 3 refit all the terms and are equivalent for one
  response; level 3 is more careful to re-balance the contributions
  from each regressor at each step and so is a little less likely to
  converge to a saddle point of the sum of squares criterion.
}
\source{
  Friedman (1984): converted to double precision and added interface to
  smoothing splines by B. D. Ripley, originally for the \CRANpkg{MASS}
  package.
}

\references{
  Friedman, J. H. and Stuetzle, W. (1981).
  Projection pursuit regression.
  \emph{Journal of the American Statistical Association},
  \bold{76}, 817--823.
  \doi{10.2307/2287576}.

  Friedman, J. H. (1984).
  SMART User's Guide.
  Laboratory for Computational Statistics, Stanford University Technical
  Report No.\sspace{}1.

  Venables, W. N. and Ripley, B. D. (2002).
  \emph{Modern Applied Statistics with S}.
  Springer.
}
\seealso{
  \code{\link{plot.ppr}}, \code{\link{supsmu}}, \code{\link{smooth.spline}}
}
\examples{
require(graphics)

# Note: your numerical values may differ
attach(rock)
area1 <- area/10000; peri1 <- peri/10000
rock.ppr <- ppr(log(perm) ~ area1 + peri1 + shape,
                data = rock, nterms = 2, max.terms = 5)
rock.ppr
# Call:
# ppr.formula(formula = log(perm) ~ area1 + peri1 + shape, data = rock,
#     nterms = 2, max.terms = 5)
#
# Goodness of fit:
#  2 terms  3 terms  4 terms  5 terms
# 8.737806 5.289517 4.745799 4.490378

summary(rock.ppr)
# .....  (same as above)
# .....
#
# Projection direction vectors ('alpha'):
#       term 1      term 2
# area1  0.34357179  0.37071027
# peri1 -0.93781471 -0.61923542
# shape  0.04961846  0.69218595
#
# Coefficients of ridge terms:
#    term 1    term 2
# 1.6079271 0.5460971

par(mfrow = c(3,2))   # maybe: , pty = "s")
plot(rock.ppr, main = "ppr(log(perm)~ ., nterms=2, max.terms=5)")
plot(update(rock.ppr, bass = 5), main = "update(..., bass = 5)")
plot(update(rock.ppr, sm.method = "gcv", gcvpen = 2),
     main = "update(..., sm.method=\"gcv\", gcvpen=2)")
cbind(perm = rock$perm, prediction = round(exp(predict(rock.ppr)), 1))
detach()
}
\keyword{regression}

