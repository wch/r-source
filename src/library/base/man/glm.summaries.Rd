\name{glm.summaries}
\alias{coefficients.glm}
\alias{family.glm}
\alias{fitted.values.glm}
\alias{residuals.glm}
\alias{print.glm}
\alias{print.glm.null}
\title{Accessing Generalized Linear Model Fits}
\usage{
\method{coefficients}{glm}(x) ; \method{coef}{glm}(x)
\method{family}{glm}(object, \dots)
\method{fitted.values}{glm}(x) ; \method{fitted}{glm}(x)
\method{residuals}{glm}(object, type = c("deviance", "pearson", "working",
                      "response", "partial"), \dots)
}
\arguments{
  \item{object, x}{an object of class \code{glm}, typically the result of
    a call to \code{\link{glm}}.}
  \item{test}{a character string, matching one of \code{"Chisq"},
    \code{"F"} or \code{"Cp"}. See \code{\link{stat.anova}}.}
  \item{type}{the type of residuals which should be returned.
    The alternatives are: \code{"deviance"} (default), \code{"pearson"},
    \code{"working"}, \code{"response"}, and \code{"partial"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  These functions are all \code{\link{methods}} for class \code{glm} or
  \code{summary.glm} objects.
}
\seealso{
  \code{\link{glm}} for computing \code{glm.obj}, \code{\link{anova.glm}};
  the corresponding \emph{generic} functions, \code{\link{summary.glm}},
  \code{\link{coefficients}}, \code{\link{deviance}},
  \code{\link{df.residual}},
  \code{\link{effects}}, \code{\link{fitted.values}},
  \code{\link{residuals}}.
}
\keyword{models}
\keyword{regression}

