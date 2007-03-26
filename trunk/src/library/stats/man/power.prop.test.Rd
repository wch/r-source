\name{power.prop.test}
\alias{power.prop.test}
\encoding{latin1}
\title{Power calculations two sample test for proportions}
\description{
  Compute power of test, or determine parameters to obtain target power.
}
\usage{
power.prop.test(n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05,
                power = NULL,
                alternative = c("two.sided", "one.sided"),
                strict = FALSE)
}
\arguments{
  \item{n}{Number of observations (per group)}
  \item{p1}{probability in one group}
  \item{p2}{probability in other group}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
  \item{alternative}{One- or two-sided test}
  \item{strict}{Use strict interpretation in two-sided case}
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
  root when invalid arguments are given. If one of them is computed
  \code{p1 < p2} will hold, although this is not enforced when both are
  specified.
}

\seealso{\code{\link{prop.test}}, \code{\link{uniroot}}}


\examples{
 power.prop.test(n = 50, p1 = .50, p2 = .75)
 power.prop.test(p1 = .50, p2 = .75, power = .90)
 power.prop.test(n = 50, p1 = .5, power = .90)
}
\keyword{ htest }
