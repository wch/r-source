% File src/library/stats/man/confint.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

% Portions copied from 

\name{confint}
\title{Confidence Intervals for Model Parameters}
\alias{confint}
\alias{confint.default}
\alias{confint.lm}
\alias{confint.glm}
\alias{confint.nls}
\description{
  Computes confidence intervals for one or more parameters in a fitted
  model.  There is a default and a method for objects inheriting from class
  \code{"\link{lm}"}.
}
\usage{
confint(object, parm, level = 0.95, \dots)
\S3method{confint}{default}(object, parm, level = 0.95, \dots)
\S3method{confint}{lm}(object, parm, level = 0.95, \dots)
\S3method{confint}{glm}(object, parm, level = 0.95, trace = FALSE, test=c("LRT", "Rao"), \dots)
\S3method{confint}{nls}(object, parm, level = 0.95, \dots)
}
\arguments{
  \item{object}{a fitted model object.}
  \item{parm}{a specification of which parameters are to be given
    confidence intervals, either a vector of numbers or a vector of
    names.  If missing, all parameters are considered.}
  \item{level}{the confidence level required.}
  \item{trace}{ logical.  Should profiling be traced?}
  \item{test}{use Likelihood Ratio or Rao Score test in profiling.}
  \item{\dots}{additional argument(s) for methods.}
}
\value{
  A matrix (or vector) with columns giving lower and upper confidence
  limits for each parameter. These will be labelled as (1-level)/2 and
  1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
}
\details{
  \code{confint} is a generic function.  The default method assumes
  normality, and needs suitable \code{\link{coef}} and
  \code{\link{vcov}} methods to be available.  The default method can be
  called directly for comparison with other methods.

  For objects of class \code{"lm"} the direct formulae based on \eqn{t}
  values are used.

  Methods for classes \code{"glm"}
  and \code{"nls"} call the appropriate profile method,
  then find the confidence intervals by interpolation in the profile
  traces.  If the profile object is already available it can be used
  as the main argument rather than the fitted model object itself.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  Original versions: \code{\link[MASS:confint]{confint.glm}} and
  \code{\link[MASS:confint]{confint.nls}} in package \CRANpkg{MASS}.
}
% the glm() example used to be wrapped in \donttest, presumably to avoid
% loading MASS (?)
\examples{
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
confint(fit)
confint(fit, "wt")

## from example(glm)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
confint(glm.D93) # needs MASS to be installed
confint.default(glm.D93)  # based on asymptotic normality}
\keyword{models}
