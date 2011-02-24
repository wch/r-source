% File src/library/stats/man/nobs.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{nobs}
\alias{nobs}
\title{
  Extract the Number of Observations from a Fit.
}
\description{
  Extract the number of \sQuote{observations} from a model fit.  This is
  principally intended to be used in computing BIC (see \code{\link{AIC}}).
}
\usage{
nobs(object, ...)
}
\arguments{
  \item{object}{A fitted model object.}
  \item{\dots}{Further arguments to be passed to methods.}
}
%% default method has use.fallback = FALSE, used in step, add1.default, drop1.default
\details{
  There are methods in this package for \code{\link{lm}},
  \code{\link{glm}}, \code{\link{nls}} and \code{\link{logLik}} objects,
  as well as a default method (which throws an error, unless argument
  \code{use.fallback = TRUE} when it looks for \code{weights} and
  \code{residuals} components -- use with care!).

  The main usage is in determining the appropriate penalty for BIC, but
  \code{nobs} is also use by the stepwise fitting methods
  \code{\link{step}}, \code{\link{add1}} and \code{\link{drop1}} as a
  quick check that different fits have been fitted to the same set of
  data (and not, say, that further rows have been dropped because of NAs
  in the new predictors).
}
\value{
  A number.
}
\seealso{
  \code{\link{AIC}}.
}
\keyword{models}
