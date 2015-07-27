% File src/library/stats/man/power.prop.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright (C) 1995-2014 R Core Team
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
  \item{alternative}{one- or two-sided test}
  \item{strict}{use strict interpretation in two-sided case}
  \item{tol}{numerical tolerance used in root finding, the default
    providing (at least) four significant digits.}
}
\details{
  Exactly one of the parameters \code{n}, \code{p1}, \code{p2},
  \code{power}, and \code{sig.level} must be passed as NULL, and that
  parameter is determined from the others. Notice that \code{sig.level}
  has a non-NULL default so NULL must be explicitly passed if you want
  it  computed.

  If \code{strict = TRUE} is used, the power will include the probability of
  rejection in the opposite direction of the true effect, in the two-sided
  case. Without this the power will be half the significance level if the
  true difference is zero.
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with \code{method} and
  \code{note} elements.
}
\author{Peter Dalgaard.  Based on previous work by Claus
  \enc{Ekstrøm}{Ekstroem}}

\note{
  \code{uniroot} is used to solve power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.  If one of them is computed
  \code{p1 < p2} will hold, although this is not enforced when both are
  specified.
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
}
\keyword{ htest }
