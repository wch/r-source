\name{simulate}
\title{Simulate responses}
\usage{
simulate(object, nsim, seed, \dots)
}
\alias{simulate}
\arguments{
  \item{object}{an object representing a fitted model.}
  \item{nsim}{number of response vectors to simulate.  Defaults to 1.}
  \item{seed}{an integer that will be used in a call to \code{set.seed}
    before simulating the response vectors.  The value is saved as the
    \code{"seed"} attribute of the returned value.  Defaults to a
    random integer between \code{0} and \code{.Machine$integer.max}.}
  \item{\dots}{additional optional arguments.}
}
\value{
  Typically, a list of length \code{nsim} of simulated response vectors.
  When appropriate the result can be a data frame (which is a special
  type of list).
  %% a *matrix* seems very natural and is more efficient
  %% for large-scale simulation, already for stats:::simulate.lm (in ../R/lm.R )
}
\description{
  Simulate one or more response vectors from the theoretical distribution
  corresponding to a fitted model object.
}
\details{
  This is a generic function. Consult the individual modeling functions
  for details on how to use this function.
}
\seealso{
  \code{\link{fitted.values}} and \code{\link{residuals}} for related methods;
  \code{\link{glm}}, \code{\link{lm}} for model fitting.
}
\examples{
x <- 1:5; simulate(lm(c(1:3,7,6) ~ x), nsim = 4)
}
\keyword{models}
\keyword{datagen}
