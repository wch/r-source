\name{anova.glm}
\title{Analysis of Deviance for  Generalized Linear Model Fits}
\usage{
anova(object, \dots, dispersion = NULL, test = NULL)
anova.glm(object, \dots, dispersion = NULL, test = NULL)
anova.glmlist(objects, dispersion = NULL, test = NULL)
}
\alias{anova.glm}
\alias{anova.glmlist}
\alias{anova.glm.null}
\description{
  Compute an analysis of deviance table for one or more generalized
  linear model fits.
}
\arguments{
  \item{object, \dots}{objects of class \code{glm}, typically
    the result of a call to \code{\link{glm}}.}
  \item{objects}{a list of \code{objects}.}
  \item{dispersion}{the dispersion parameter for the fitting family.
    By default it is obtained from \code{glm.obj}.}
  \item{test}{a character string, (partially) matching one of \code{"Chisq"},
    \code{"F"} or \code{"Cp"}. See \code{\link{stat.anova}}.}
}
\details{
  Specifying a single object gives a sequential analysis of deviance
  table for that fit.  That is, the reductions in the residual deviance
  as each term of the formula is added in turn are given in as
  the rows of a table, plus the residual deviances themselves.

  If more than one object is specified, the table has a row for the
  residual degrees of freedom and deviance for each model.
  For all but the first model, the change in degrees of freedom and
  deviance is also given. (This only make statistical sense if the
  models are nested.)  It is conventional to list the models from
  smallest to largest, but this is up to the user.

  The table will optionally contain test statistics (and P values)
  comparing the reduction in deviance for the row to the residuals.
  For models with known dispersion (e.g. binomial and Poisson fits)
  the chi-squared test is most appropriate, and for those with
  dispersion estimated by moments (e.g. \code{gaussian},
  \code{pseudobinomial} and \code{pseudopoisson} fits) the F test is
  most appropriate.  Mallows' \eqn{C_p}{Cp} statistic is the residual
  deviance plus twice the estimate of \eqn{\sigma^2}{sigma^2} times
  the residual degrees of freedom, which is closely related to AIC (and
  a multiple of it if the dispersion is known).
}
\value{
  An object of class \code{"anova"} inheriting from class \code{"data.frame"}.
}
\section{Warning}{
  The comparison between two or more models by \code{anova} or
  \code{anova.glmlist} will only be valid if they
  are fitted to the same dataset. This may be a problem if there are
  missing values and \R's default of \code{na.action = na.omit} is used,
  and \code{anova.glmlist} will detect this with an error.
}
\seealso{
  \code{\link{glm}}, \code{\link{anova}}.
}
\examples{
## --- Continuing the Example from  ``?glm'':%\code{\link{glm}}:
\testonly{example("glm", echo = FALSE)}
anova(glm.D93)
anova(glm.D93, test = "Cp")
anova(glm.D93, test = "Chisq")
}
\keyword{models}
\keyword{regression}

