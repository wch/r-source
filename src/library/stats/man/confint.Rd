\name{confint}
\alias{confint}
%\alias{confint.lm}
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
  \code{confint} is a generic function with no default method.
  For objects of class \code{"lm"} the direct formulae based on t values
  are used.

  Package \pkg{MASS} contains methods for \code{"glm"} and \code{"nls"}
  objects.
}
\seealso{
  \code{\link[MASS]{confint.nls}} in package \pkg{MASS}.
}
\examples{
fit <- lm(100/mpg ~ disp + hp + wt + am, data=mtcars)
confint(fit)
confint(fit, "wt")
}
\keyword{models}
