% File src/library/stats/man/nobs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{nobs}
\alias{nobs}
\alias{nobs.default}
\title{
  Extract the Number of Observations from a Fit.
}
\description{
  Extract the number of \sQuote{observations} from a model fit.  This is
  principally intended to be used in computing BIC (see \code{\link{AIC}}).
}
\usage{
nobs(object, ...)

\method{nobs}{default}(object, use.fallback = FALSE, ...)
}
\arguments{
  \item{object}{A fitted model object.}
  \item{use.fallback}{logical: should fallback methods be used to try to
    guess the value?}
  \item{\dots}{Further arguments to be passed to methods.}
}
\details{
  This is a generic function, with an S4 generic in package \pkg{stats4}.
  There are methods in this package for objects of classes
  \code{"\link{lm}"}, \code{"\link{glm}"}, \code{"\link{nls}"} and
  \code{"\link{logLik}"}, as well as a default method (which throws an
  error, unless \code{use.fallback = TRUE} when it looks for
  \code{weights} and \code{residuals} components -- use with care!).

  The main usage is in determining the appropriate penalty for BIC, but
  \code{nobs} is also used by the stepwise fitting methods
  \code{\link{step}}, \code{\link{add1}} and \code{\link{drop1}} as a
  quick check that different fits have been fitted to the same set of
  data (and not, say, that further rows have been dropped because of NAs
  in the new predictors).

  For \code{lm}, \code{glm} and \code{nls} fits, observations with zero
  weight are not included.
}
\value{
  A single number, normally an integer.  Could be \code{NA}.
}
\seealso{
  \code{\link{AIC}}.
}
\keyword{models}
