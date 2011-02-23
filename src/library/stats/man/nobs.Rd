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
\details{
  There are methods in this package for \code{\link{lm}},
  \code{\link{glm}}, \code{\link{nls}} and \code{\link{logLik}} objects,
  as well as a default method (which throws an error).
}
\value{
  A number.
}
\seealso{
  \code{\link{AIC}}.
}
\keyword{models}
