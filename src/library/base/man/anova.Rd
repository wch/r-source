\name{anova}
\title{Anova Tables}
\usage{
anova(object, \dots)
print(anova.object)
}
\alias{anova}
\alias{anova.mlm}
% others in ./lm.summaries.Rd
\alias{print.anova}
\description{
  Compute analysis of variance (or deviance) tables for one or mode
  fitted model objects.
}
\arguments{
  \item{object}{an object containing the results returned
    by a model fitting function (e.g. \code{lm} or \code{glm}).}
  \item{\dots}{additional objects of the same type.}
}
\value{
  This (generic) function returns an object of class \code{anova}.
  These objects represent analysis-of-variance and analysis-of-deviance tables.
  When given a single argument it produces a table which
  tests whether the model terms are significant.

  When given a sequence of objects, \code{anova} tests
  the models against one another in the order specified.

  The print method for \code{anova} objects prints
  tables in a ``pretty'' form.
}
\seealso{
  \code{\link{coefficients}}, \code{\link{effects}},
  \code{\link{fitted.values}}, \code{\link{residuals}}, \code{\link{summary}}.
}
\keyword{regression}
\keyword{models}
