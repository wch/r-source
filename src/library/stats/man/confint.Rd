\name{confint}
\alias{confint}
\alias{confint.default}
\title{Confidence Intervals for Model Parameters}
\description{
  Computes confidence intervals for one or more parameters in a fitted
  model.  Base has a method for objects inheriting from class
  \code{"lm"}.
}
\usage{
confint(object, parm, level = 0.95, \dots)
}
\arguments{
\item{object}{
a fitted model object.
}
\item{parm}{
a specification of which parameters are to be given confidence
intervals, either a vector of numbers or a vector of names. If
missing, all parameters are considered.
}
\item{level}{
the confidence level required.
}
\item{\dots}{
additional argument(s) for methods
}}
\value{
A matrix (or vector) with columns giving lower and upper confidence
limits for each parameter. These will be labelled as (1-level)/2 and
1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
}
\details{
  \code{confint} is a generic function.  The default method assumes
  asymptotic nornality, and needs suitable \code{\link{coef}} and
  \code{\link{vcov}} methods to be available.  The default method can be
  called directly for comparison with other methods.

  For objects of class \code{"lm"} the direct formulae based on \eqn{t}
  values are used.

  There are stub methods for classes \code{"glm"} and \code{"nls"} which
  invoke those in package \pkg{MASS} which are based on profile
  likelihoods.
}
\seealso{
  \code{\link[MASS]{confint.glm}} and \code{\link[MASS]{confint.nls}} in
  package \pkg{MASS}.
}
\examples{
fit <- lm(100/mpg ~ disp + hp + wt + am, data=mtcars)
confint(fit)
confint(fit, "wt")

## from example(glm)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9); treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
confint(glm.D93)
confint.default(glm.D93)  # based on asympotic normality
}
\keyword{models}
