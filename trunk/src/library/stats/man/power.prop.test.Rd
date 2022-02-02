% File src/library/stats/man/power.prop.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright (C) 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{power.prop.test}
\alias{power.prop.test}
\encoding{UTF-8}
\title{Power Calculations for Two-Sample Test for Proportions}
\description{
  Compute the power of the two-sample test for proportions, or determine
  parameters to obtain a target power.
}
\usage{
power.prop.test(n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05,
                power = NULL,
                alternative = c("two.sided", "one.sided"),
                strict = FALSE, tol = .Machine$double.eps^0.25)
}
\arguments{
  \item{n}{number of observations (per group)}
  \item{p1}{probability in one group}
  \item{p2}{probability in other group}
  \item{sig.level}{significance level (Type I error probability)}
  \item{power}{power of test (1 minus Type II error probability)}
  \item{alternative}{one- or two-sided test.  Can be abbreviated.}
  \item{strict}{use strict interpretation in two-sided case}
  \item{tol}{numerical tolerance used in root finding, the default
    providing (at least) four significant digits.}
}
\details{
  Exactly one of the parameters \code{n}, \code{p1}, \code{p2},
  \code{power}, and \code{sig.level} must be passed as NULL, and that
  parameter is determined from the others.  Notice that \code{sig.level}
  has a non-NULL default so \code{NULL} must be explicitly passed if you
  want it computed.

  If \code{strict = TRUE} is used, the power will include the probability of
  rejection in the opposite direction of the true effect, in the two-sided
  case.  Without this the power will be half the significance level if the
  true difference is zero.

  Note that not all conditions can be satisfied, e.g., for \preformatted{power.prop.test(n=30, p1=0.90, p2=NULL, power=0.8, strict=TRUE)}
  there is no proportion \code{p2} between \code{p1 = 0.9} and 1, as
  you'd need a sample size of at least \eqn{n = 74} to yield the
  desired power for \eqn{(p1,p2) = (0.9, 1)}.

  For these impossible conditions, currently a warning
  (\code{\link{warning}}) is signalled which may become an error
  (\code{\link{stop}}) in the future.
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with \code{method} and
  \code{note} elements.
}
\author{Peter Dalgaard.  Based on previous work by Claus
  \enc{Ekstrøm}{Ekstroem}}

\note{
  \code{\link{uniroot}} is used to solve power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.   If one of \code{p1} and
  \code{p2} is computed, then \eqn{p1 < p2} is assumed and will hold,
  but if you specify both, \eqn{p2 \le p1}{p2 <= p1} is allowed.
}

\seealso{\code{\link{prop.test}}, \code{\link{uniroot}}}

\examples{
power.prop.test(n = 50, p1 = .50, p2 = .75)      ## => power = 0.740
power.prop.test(p1 = .50, p2 = .75, power = .90) ## =>     n = 76.7
power.prop.test(n = 50, p1 = .5, power = .90)    ## =>    p2 = 0.8026
power.prop.test(n = 50, p1 = .5, p2 = 0.9, power = .90, sig.level=NULL)
                                                 ## => sig.l = 0.00131
power.prop.test(p1 = .5, p2 = 0.501, sig.level=.001, power=0.90)
                                                 ## => n = 10451937
try(
 power.prop.test(n=30, p1=0.90, p2=NULL, power=0.8)
) # a warning  (which may become an error)
## Reason:
power.prop.test(      p1=0.90, p2= 1.0, power=0.8) ##-> n = 73.37
}
\keyword{ htest }
