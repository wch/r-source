% File src/library/stats/man/power.anova.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{power.anova.test}
\alias{power.anova.test}
\encoding{UTF-8}
\title{Power Calculations for Balanced One-Way Analysis of Variance Tests}
\usage{
power.anova.test(groups = NULL, n = NULL,
                 between.var = NULL, within.var = NULL,
                 sig.level = 0.05, power = NULL)
}
\arguments{
 \item{groups}{Number of groups}
 \item{n}{Number of observations (per group)}
 \item{between.var}{Between group variance}
 \item{within.var}{Within group variance}
 \item{sig.level}{Significance level (Type I error probability)}
 \item{power}{Power of test (1 minus Type II error probability)}
}
\description{
 Compute power of test or determine parameters to obtain target power.
}

\details{
  Exactly one of the parameters \code{groups}, \code{n}, \code{between.var},
  \code{power}, \code{within.var}, and \code{sig.level} must be passed as NULL,
  and that parameter is determined from the others. Notice that
  \code{sig.level} has non-NULL default so NULL must be explicitly
  passed if you want it computed.
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with \code{method} and
  \code{note} elements.
}

\author{Claus \enc{Ekstrøm}{Ekstroem}}
\note{
  \code{uniroot} is used to solve power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.
}
\seealso{\code{\link{anova}}, \code{\link{lm}}, \code{\link{uniroot}}}

\examples{
power.anova.test(groups = 4, n = 5, between.var = 1, within.var = 3)
# Power = 0.3535594

power.anova.test(groups = 4, between.var = 1, within.var = 3,
                 power = .80)
# n = 11.92613

## Assume we have prior knowledge of the group means:
groupmeans <- c(120, 130, 140, 150)
power.anova.test(groups = length(groupmeans),
                 between.var = var(groupmeans),
                 within.var = 500, power = .90) # n = 15.18834
}
\keyword{htest}
