% File src/library/stats/man/glm.summaries.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{glm.summaries}
\alias{family.glm}
\alias{residuals.glm}
\title{Accessing Generalized Linear Model Fits}
\usage{
\method{family}{glm}(object, \dots)

\method{residuals}{glm}(object, type = c("deviance", "pearson", "working",
                           "response", "partial"), \dots)
}
\arguments{
  \item{object}{an object of class \code{glm}, typically the result of
    a call to \code{\link{glm}}.}
  \item{type}{the type of residuals which should be returned.
    The alternatives are: \code{"deviance"} (default), \code{"pearson"},
    \code{"working"}, \code{"response"}, and \code{"partial"}.
    Can be abbreviated.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  These functions are all \code{\link{methods}} for class \code{glm} or
  \code{summary.glm} objects.
}
\details{
  The references define the types of residuals:
  \bibcitet{R:Davison+Snell:1991} is a good reference for the usages of
  each. 

  The partial residuals are a matrix of working residuals, with each
  column formed by omitting a term from the model.

  How \code{residuals} treats cases with missing values in the original
  fit is determined by the \code{na.action} argument of that fit.
  If \code{na.action = na.omit} omitted cases will not appear in the
  residuals, whereas if \code{na.action = na.exclude} they will appear,
  with residual value \code{NA}.  See also \code{\link{naresid}}.

  For fits done with \code{y = FALSE} the response values are computed
  from other components.
}
\seealso{
  \code{\link{glm}} for computing \code{glm.obj}, \code{\link{anova.glm}};
  the corresponding \emph{generic} functions, \code{\link{summary.glm}},
  \code{\link{coef}}, \code{\link{deviance}},
  \code{\link{df.residual}},
  \code{\link{effects}}, \code{\link{fitted}},
  \code{\link{residuals}}.

  \link{influence.measures} for deletion diagnostics, including
  standardized (\code{\link{rstandard}})
  and studentized (\code{\link{rstudent}}) residuals.
}
\references{
  \bibshow{*, R:McCullagh+Nelder:1989}

  \bibshow{R:Hastie+Pregibon:1992}}
\keyword{models}
\keyword{regression}

