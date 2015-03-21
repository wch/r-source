% File src/library/stats/man/power.t.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright (C) 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{power.t.test}
\alias{power.t.test}
\encoding{UTF-8}
\title{Power calculations for one and two sample t tests}
\usage{
power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05,
             power = NULL,
             type = c("two.sample", "one.sample", "paired"),
             alternative = c("two.sided", "one.sided"),
             strict = FALSE, tol = .Machine$double.eps^0.25)
}
\arguments{
 \item{n}{number of observations (per group)}
 \item{delta}{true difference in means}
 \item{sd}{standard deviation}
 \item{sig.level}{significance level (Type I error probability)}
 \item{power}{power of test (1 minus Type II error probability)}
 \item{type}{string specifying the type of t test.  Can be abbreviated.}
 \item{alternative}{one- or two-sided test.  Can be abbreviated.}
 \item{strict}{use strict interpretation in two-sided case}
  \item{tol}{numerical tolerance used in root finding, the default
    providing (at least) four significant digits.}
}
\description{
  Compute the power of the one- or two- sample t test,
  or determine parameters to obtain a target power.
}

\details{
  Exactly one of the parameters \code{n}, \code{delta}, \code{power},
  \code{sd}, and \code{sig.level} must be passed as \code{NULL}, and that
  parameter is determined from the others.  Notice that the last two have
  non-NULL defaults, so NULL must be explicitly passed if you want to
  compute them.

  If \code{strict = TRUE} is used, the power will include the probability of
  rejection in the opposite direction of the true effect, in the two-sided
  case.  Without this the power will be half the significance level if the
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
  \code{uniroot} is used to solve the power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.
}
\seealso{\code{\link{t.test}}, \code{\link{uniroot}}}

\examples{
 power.t.test(n = 20, delta = 1)
 power.t.test(power = .90, delta = 1)
 power.t.test(power = .90, delta = 1, alternative = "one.sided")
}
\keyword{htest}


