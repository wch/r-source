\name{glm.control}
\alias{glm.control}
\title{Auxiliary for Controlling GLM Fitting}
\description{
  Auxiliary function as user interface for \code{\link{glm}} fitting.
  Typically only used when calling \code{\link{glm}} or \code{\link{glm.fit}}.
}
\usage{
glm.control(epsilon = 1e-8, maxit = 25, trace = FALSE)
}
\arguments{
  \item{epsilon}{positive convergence tolerance \eqn{\epsilon};
    the iterations converge when
    \eqn{|dev - dev_{old}|/(|dev| + 0.1) < \epsilon}{|dev - devold|/(|dev| + 0.1) < \epsilon}.}
  \item{maxit}{integer giving the maximal number of IWLS iterations.}
  \item{trace}{logical indicating if output should be produced for each
    iteration.}
}
\details{
  If \code{epsilon} is small, it is also used as the tolerance for the
  least squares solution.

  When \code{trace} is true, calls to \code{\link{cat}} produce the
  output for each IWLS iteration. Hence, \code{\link{options}(digits = *)}
  can be used to increase the precision, see the example.
}
\value{
  A list with the arguments as components.
}
\references{
  Hastie, T. J. and Pregibon, D. (1992)
  \emph{Generalized linear models.}
  Chapter 6 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{glm.fit}}, the fitting procedure used by
  \code{\link{glm}}.}
\examples{
### A variation on  example(glm) :

## Annette Dobson's example ...
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
oo <- options(digits = 12) # to see more when tracing :
glm.D93X <- glm(counts ~ outcome + treatment, family=poisson(),
                trace = TRUE, epsilon = 1e-14)
options(oo)
coef(glm.D93X) # the last two are closer to 0 than in ?glm's  glm.D93
}
\keyword{optimize}
\keyword{models}

