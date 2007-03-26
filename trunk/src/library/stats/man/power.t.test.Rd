\name{power.t.test}
\alias{power.t.test}
\encoding{latin1}
\title{Power calculations for one and two sample t tests}
\usage{
power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05,
             power = NULL,
             type = c("two.sample", "one.sample", "paired"),
             alternative = c("two.sided", "one.sided"),
             strict = FALSE)
}
\arguments{
 \item{n}{Number of observations (per group)}
 \item{delta}{True difference in means}
 \item{sd}{Standard deviation}
 \item{sig.level}{Significance level (Type I error probability)}
 \item{power}{Power of test (1 minus Type II error probability)}
 \item{type}{Type of t test}
 \item{alternative}{One- or two-sided test}
 \item{strict}{Use strict interpretation in two-sided case}
}
\description{
 Compute power of test, or determine parameters to obtain target power.
}

\details{
  Exactly one of the parameters \code{n}, \code{delta}, \code{power},
  \code{sd}, and \code{sig.level} must be passed as NULL, and that
  parameter is determined from the others. Notice that the last two have
  non-NULL defaults so NULL must be explicitly passed if you want to
  compute them.

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
  \enc{Ekstrøm}{Ekstrom}}

\note{
  \code{uniroot} is used to solve power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.
}
\seealso{\code{\link{t.test}}, \code{\link{uniroot}}}

\examples{
 power.t.test(n = 20, delta = 1)
 power.t.test(power = .90, delta = 1)
 power.t.test(power = .90, delta = 1, alt = "one.sided")
}
\keyword{htest}


