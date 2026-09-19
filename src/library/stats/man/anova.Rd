% File src/library/stats/man/anova.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{anova}
\title{ANOVA Tables}
\alias{anova}
\alias{print.anova}
\concept{regression}
\usage{
anova(object, \dots)

\S3method{print}{anova}(x,
      digits = max(getOption("digits") - 2L, 3L),
      signif.stars = getOption("show.signif.stars"), \dots)
}
\description{
  Compute analysis of variance (or deviance) tables for one or more
  fitted model objects.
}
\arguments{
  \item{object}{an object containing the results returned by a model
    fitting function (e.g., \code{lm} or \code{glm}).}
  \item{\dots}{For \code{anova()}, additional objects of the same type; for
    the \code{print()} method, optional arguments passed to
    \code{\link{printCoefmat}()}.}
  \item{x}{an \code{"anova"} object, typically resulting from \code{anova(..)}.}
  \item{digits, signif.stars}{optional; passed to \code{\link{printCoefmat}()}.}
}
\value{
  This (generic) function returns an object of class \code{anova}.
  These objects represent analysis-of-variance and analysis-of-deviance tables.
  When given a single argument it produces a table which
  tests whether the model terms are significant.

  When given a sequence of objects, \code{anova} tests
  the models against one another in the order specified.

  The print method for \code{anova} objects prints tables in a
  \sQuote{pretty} form.
}
\section{Warning}{
  The comparison between two or more models will only be valid if they
  are fitted to the same dataset. This may be a problem if there are
  missing values and \R's default of \code{na.action = na.omit} is used.
}
\references{
  \bibshow{R:Chambers+Hastie:1992}
}
\seealso{The separate help pages for the \code{"lm"} and \code{"glm"} methods,
  \code{\link{anova.lm}}, and \code{\link{anova.glm}} respectively.  Further,
  \code{\link{coefficients}}, \code{\link{effects}},
  \code{\link{fitted.values}}, \code{\link{residuals}},
  \code{\link{summary}}, \code{\link{drop1}}, \code{\link{add1}}.
}
\keyword{regression}
\keyword{models}
