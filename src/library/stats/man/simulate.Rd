\name{simulate}
\title{Simulate Responses}
\usage{
simulate(object, nsim, seed, \dots)
}
\alias{simulate}
\arguments{
  \item{object}{an object representing a fitted model.}
  \item{nsim}{number of response vectors to simulate.  Defaults to 1.}
  \item{seed}{an object specifying if and how the random number
    generator should be initialized (\dQuote{seeded}).\cr
    For the "lm" method, either \code{NULL} or an integer that will be
    used in a call to \code{set.seed} before simulating the response
    vectors.  If set, the value is saved as the \code{"seed"} attribute
    of the returned value.  The default, \code{NULL} will not change the
    random generator state, and return \code{\link{.Random.seed}} as
    \code{"seed"} attribute, see below.
  }
  \item{\dots}{additional optional arguments.}
}
\value{
  Typically, a list of length \code{nsim} of simulated response vectors.
  When appropriate the result can be a data frame (which is a special
  type of list).
  %% a *matrix* seems very natural and is more efficient
  %% for large-scale simulation, already for stats:::simulate.lm (in ../R/lm.R )

  For the \code{"lm"} method, the result is a data frame with an
  attribute \code{"seed"} containing the \code{seed} argument and
  \code{as.list(\link{RNGkind}())} if \code{seed} was not \code{NULL},
  or the value of \code{\link{.Random.seed}} before the simulation was
  started when \code{seed} was NULL as by default.
}
\description{
  Simulate one or more response vectors from the theoretical distribution
  corresponding to a fitted model object.
}
\details{
  This is a generic function with a method for \code{\link{lm}} objects.
  Consult the individual modeling functions
  for details on how to use this function.
}
\seealso{
  \code{\link{fitted.values}} and \code{\link{residuals}} for related methods;
  \code{\link{glm}}, \code{\link{lm}} for model fitting.
}
\examples{
x <- 1:5
mod1 <- lm(c(1:3,7,6) ~ x)
S1 <- simulate(mod1, nsim = 4)
## repeat the simulation:
.Random.seed <- attr(S1, "seed")
identical(S1, simulate(mod1, nsim = 4))

S2 <- simulate(mod1, nsim = 200, seed = 101)
rowMeans(S2) # should be about
fitted(mod1)

## repeat identically:
(sseed <- attr(S2, "seed")) # seed; RNGkind as attribute
stopifnot(identical(S2, simulate(mod1, nsim = 200, seed = sseed)))

## To be sure about the proper RNGkind, e.g., after
RNGversion("1.0.0")
## first set the RNG kind, then simulate
do.call(RNGkind, attr(sseed, "kind"))
identical(S2, simulate(mod1, nsim = 200, seed = sseed))
}
\keyword{models}
\keyword{datagen}
