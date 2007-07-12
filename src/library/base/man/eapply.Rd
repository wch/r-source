\name{eapply}
\title{Apply a Function over values in an environment}
\usage{
eapply(env, FUN, \dots, all.names = FALSE)
}
\alias{eapply}
\arguments{
  \item{env}{environment to be used.}
  \item{FUN}{the function to be applied, found \emph{via}
    \code{\link{match.fun}}.
    In the case of functions like \code{+},
    \code{\%*\%}, etc., the function name must be backquoted or quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{all.names}{a logical indicating whether to apply the function to
    all values}
}
\description{
  \code{eapply} applies \code{FUN} to the named values from an
  environment and returns the results as a list.  The user can request
  that all named objects are used (normally names that begin with a
  dot are not). The output is not sorted and no parent environments are
  searched.
}
\seealso{
  \code{\link{lapply}}.
}
\examples{
require(utils); require(stats)

env <- new.env()
env$a <- 1:10
env$beta <- exp(-3:3)
env$logic <- c(TRUE,FALSE,FALSE,TRUE)
# what have we there?
eapply(env, str)

# compute the mean for each list element
eapply(env, mean)
# median and quartiles for each list element
eapply(env, quantile, probs = 1:3/4)
eapply(env, quantile)
}
\keyword{iteration}
\keyword{environment}
\keyword{list}
