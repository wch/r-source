% File src/library/stats/man/mood.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{mood.test}
\alias{mood.test}
\alias{mood.test.default}
\alias{mood.test.formula}
\title{Mood Two-Sample Test of Scale}
\description{
  Performs Mood's two-sample test for a difference in scale parameters.
}
\usage{
mood.test(x, \dots)

\method{mood.test}{default}(x, y,
          alternative = c("two.sided", "less", "greater"), \dots)

\method{mood.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x, y}{numeric vectors of data values.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"} (default), \code{"greater"} or
    \code{"less"} all of which can be abbreviated.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} a factor
    with two levels giving the corresponding groups.}
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
  The underlying model is that the two samples are drawn from
  \eqn{f(x-l)} and \eqn{f((x-l)/s)/s}, respectively, where \eqn{l} is a
  common location parameter and \eqn{s} is a scale parameter.

  The null hypothesis is \eqn{s = 1}.

  There are more useful tests for this problem.

  In the case of ties, the formulation of Mielke (1967) is employed.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.   You can specify just the initial letter.}
  \item{method}{the character string \code{"Mood two-sample test of scale"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  William J. Conover (1971),
  \emph{Practical nonparametric statistics}.
  New York: John Wiley & Sons.
  Pages 234f.

  Paul W. Mielke, Jr. (1967).
  Note on some squared rank tests with existing ties.
  \emph{Technometrics}, \bold{9}/2, 312--314.
  \doi{10.2307/1266427}.
}
\seealso{
  \code{\link{fligner.test}} for a rank-based (nonparametric) k-sample
  test for homogeneity of variances;
  \code{\link{ansari.test}} for another rank-based two-sample test for a
  difference in scale parameters;
  \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric
  tests for the homogeneity in variance.
}
\examples{
## Same data as for the Ansari-Bradley test:
## Serum iron determination using Hyland control sera
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
            101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
            100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
mood.test(ramsay, jung.parekh)
## Compare this to ansari.test(ramsay, jung.parekh)
}
\keyword{htest}
