% File src/library/stats/man/oneway.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{oneway.test}
\alias{oneway.test}
\title{Test for Equal Means in a One-Way Layout}
\description{
  Test whether two or more samples from normal distributions have the
  same means.  The variances are not necessarily assumed to be equal.
}
\usage{
oneway.test(formula, data, subset, na.action, var.equal = FALSE)
}
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{var.equal}{a logical variable indicating whether to treat the
    variances in the samples as equal.  If \code{TRUE}, then a simple F
    test for the equality of means in a one-way analysis of variance is
    performed.  If \code{FALSE}, an approximate method of Welch (1951)
    is used, which generalizes the commonly known 2-sample Welch test to
    the case of arbitrarily many samples.}
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom of the exact or approximate F
    distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a character string indicating the test performed.}
  \item{data.name}{a character string giving the names of the data.}
}
\description{
  If the right-hand side of the formula contains more than one term,
  their interaction is taken to form the grouping.
}
\references{
  B. L. Welch (1951),
  On the comparison of several mean values: an alternative approach.
  \emph{Biometrika}, \bold{38}, 330--336.
}
\seealso{
  The standard t test (\code{\link{t.test}}) as the special case for two
  samples;
  the Kruskal-Wallis test \code{\link{kruskal.test}} for a nonparametric
  test for equal location parameters in a one-way layout.
}
\examples{
## Not assuming equal variances
oneway.test(extra ~ group, data = sleep)
## Assuming equal variances
oneway.test(extra ~ group, data = sleep, var.equal = TRUE)
## which gives the same result as
anova(lm(extra ~ group, data = sleep))
}
\keyword{htest}
