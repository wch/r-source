\name{anova}
\alias{anova}
\alias{print.anova}
\concept{regression}
\title{Anova Tables}
\usage{
anova(object, \dots)
}
\description{
  Compute analysis of variance (or deviance) tables for one or more
  fitted model objects.
}
\arguments{
  \item{object}{an object containing the results returned by a model
    fitting function (e.g., \code{lm} or \code{glm}).} 
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
  tables in a \dQuote{pretty} form.
}
\section{Warning}{
  The comparison between two or more models will only be valid if they
  are fitted to the same dataset. This may be a problem if there are
  missing values and \R's default of \code{na.action = na.omit} is used.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{coefficients}}, \code{\link{effects}},
  \code{\link{fitted.values}}, \code{\link{residuals}},
  \code{\link{summary}}, \code{\link{drop1}}, \code{\link{add1}}.
}
\keyword{regression}
\keyword{models}
