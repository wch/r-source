\name{glm.summaries}
\title{Accessing Generalized Linear Model Fits}
\usage{
summary(object, dispersion = NULL, correlation = FALSE, \dots)
anova(object, dispersion = NULL, \dots, test = NULL)
anova.glmlist(object, dispersion = NULL, test = NULL, \dots)
coefficients(x) ; coef(x)
df.residual(x)
family(x)
fitted.values(x) ; fitted(x)
residuals(x, type = c("deviance", "pearson", "working",
                      "response", "partial"), \dots)
print.summary(summary.glm.obj, digits = max(3, getOption("digits") - 3),
              na.print = "", symbolic.cor = p > 4,
              signif.stars = getOption("show.signif.stars"), \dots)
}
\alias{summary.glm}
\alias{summary.glm.null}
\alias{anova.glm}
\alias{anova.glmlist}
\alias{anova.glm.null}
\alias{coefficients.glm}
\alias{df.residual.glm}
\alias{family.glm}
\alias{fitted.values.glm}
\alias{residuals.glm}
\alias{print.glm}
\alias{print.glm.null}
%Defunct: print.anova.glm
\alias{print.summary.glm}
\alias{print.summary.glm.null}
\arguments{
  \item{object, x}{an object of class \code{glm}, typically the result of
    a call to \code{\link{glm}}.}
  \item{dispersion}{the dispersion parameter for the fitting family.
    By default it is obtained from \code{glm.obj}.}
  \item{correlation}{logical; if \code{TRUE}, the correlation matrix of
    the estimated parameters is returned and printed.}
  \item{test}{a character string, matching one of \code{"Chisq"},
    \code{"F"} or \code{"Cp"}. See \code{\link{stat.anova}}.}
  \item{type}{the type of residuals which should be returned.
    The alternatives are: \code{"deviance"} (default), \code{"pearson"},
    \code{"working"}, \code{"response"}, and \code{"partial"}.}
}
\description{
  These functions are all \code{\link{methods}} for class \code{glm} or
  \code{summary.glm} and \code{anova.glm} objects.

  \code{anova.glmlist} is called from \code{anova.glm} for comparison of
  several \code{glm} models.
}
\value{
  The function \code{summary} can be used to obtain or print
  a summary of the results and the function \code{anova}
  and be used to produce and analysis of variance table.
}
\section{Warning}{
  The comparison between two or more models by \code{anova} or
  \code{anova.glmlist} will only be valid if they
  are fitted to the same dataset. This may be a problem if there are
  missing values and \R's default of \code{na.action = na.omit} is used.
}
\seealso{
  \code{\link{glm}} for computing \code{glm.obj};
  the corresponding \emph{generic} functions, \code{\link{summary}},
  \code{\link{anova}}, \code{\link{coefficients}}, \code{\link{deviance}},
  \code{\link{effects}}, \code{\link{fitted.values}}, \code{\link{residuals}}.
}
\examples{
## --- Continuing the Example from  ``?glm'':%\code{\link{glm}}:
\testonly{example("glm", echo = FALSE)}
summary(glm.D93)
anova(glm.D93)
anova(glm.D93, test = "Cp")
anova(glm.D93, test = "Chisq")
}
\keyword{models}
\keyword{regression}

