% File src/library/stats/man/bartlett.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{bartlett.test}
\alias{bartlett.test}
\alias{bartlett.test.default}
\alias{bartlett.test.formula}
\title{Bartlett Test of Homogeneity of Variances}
\description{
  Performs Bartlett's test of the null that the variances in each of the
  groups (samples) are the same.
}
\usage{
bartlett.test(x, \dots)

\method{bartlett.test}{default}(x, g, \dots)

\method{bartlett.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors representing the respective samples, or fitted linear model
    objects (inheriting from class \code{"lm"}).}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.
    Ignored if \code{x} is a list.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    gives the data values and \code{rhs} the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  If \code{x} is a list, its elements are taken as the samples or fitted
  linear models to be compared for homogeneity of variances.  In this
  case, the elements must either all be numeric data vectors or fitted
  linear model objects, \code{g} is ignored, and one can simply use
  \code{bartlett.test(x)} to perform the test.  If the samples are not
  yet contained in a list, use \code{bartlett.test(list(x, ...))}.

  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving the
  group for the corresponding elements of \code{x}.
}
\value{
  A list of class \code{"htest"} containing the following components:
  \item{statistic}{Bartlett's K-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate chi-squared
    distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string
    \code{"Bartlett test of homogeneity of variances"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\seealso{
  \code{\link{var.test}} for the special case of comparing variances in
  two samples from normal distributions;
  \code{\link{fligner.test}} for a rank-based (nonparametric)
  \eqn{k}-sample test for homogeneity of variances;
  \code{\link{ansari.test}} and \code{\link{mood.test}} for two rank
  based two-sample tests for difference in scale.
}
\references{
  Bartlett, M. S. (1937).
  Properties of sufficiency and statistical tests.
  \emph{Proceedings of the Royal Society of London Series A}
  \bold{160}, 268--282.
}

% FIXME: could use something more interesting here
\examples{
require(graphics)

plot(count ~ spray, data = InsectSprays)
bartlett.test(InsectSprays$count, InsectSprays$spray)
bartlett.test(count ~ spray, data = InsectSprays)
}
\keyword{htest}
