\name{stat.anova}
\title{GLM Anova Statistics}
\usage{
stat.anova(table, test, scale, df.scale, n)
}
\alias{stat.anova}
\arguments{
 \item{table}{numeric matrix as results from
   \code{\link{anova.glm}(\dots, test=NULL)}.}
 \item{test}{a character string, matching one of \code{"Chisq"},
   \code{"F"} or \code{"Cp"}.}
 \item{scale}{a weighted residual sum of squares.}
 \item{df.scale}{degrees of freedom corresponding to scale.}
 \item{n}{number of observations.}
}
\description{This is a utility function, used in \code{lm} and
  \code{glm} methods for \code{\link{anova}(\dots, test != NULL)}
  and should not be used by the average user.
}
\value{
  A matrix which is the original \code{table}, augmented by a column
  of test statistics, depending on the \code{test} argument.
}
\seealso{\code{\link{anova.lm}}, \code{\link{anova.glm}}.}
\examples{
##-- Continued from  ``?glm'':
\testonly{example("glm", echo = FALSE)}
print(ag <- anova(glm.D93))
stat.anova(ag$table, test = "Cp",
           scale = sum(resid(glm.D93, "pearson")^2)/4, df = 4, n = 9)
}
\keyword{regression}
\keyword{models}
