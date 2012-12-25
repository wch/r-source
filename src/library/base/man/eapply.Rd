% File src/library/base/man/eapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{eapply}
\title{Apply a Function Over Values in an Environment}
\usage{
eapply(env, FUN, \dots, all.names = FALSE, USE.NAMES = TRUE)
}
\alias{eapply}
\arguments{
  \item{env}{environment to be used.}
  \item{FUN}{the function to be applied, found \emph{via}
    \code{\link{match.fun}}.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be backquoted or quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{all.names}{a logical indicating whether to apply the function to
    all values.}
  \item{USE.NAMES}{logical indicating whether the resulting list should
    have \code{\link{names}}.}
}
\description{
  \code{eapply} applies \code{FUN} to the named values from an
  \code{\link{environment}} and returns the results as a list.  The user
  can request that all named objects are used (normally names that begin
  with a dot are not).  The output is not sorted and no enclosing
  environments are searched.

  This is a \link{primitive} function.
}
\value{
  A named (unless \code{USE.NAMES = FALSE}) list.  Note that the order of
  the components is arbitrary for hashed environments.
}
\seealso{
  \code{\link{environment}}, \code{\link{lapply}}.
}
\examples{
require(stats)

env <- new.env(hash = FALSE) # so the order is fixed
env$a <- 1:10
env$beta <- exp(-3:3)
env$logic <- c(TRUE, FALSE, FALSE, TRUE)
# what have we there?
utils::ls.str(env)

# compute the mean for each list element
       eapply(env, mean)
unlist(eapply(env, mean, USE.NAMES = FALSE))

# median and quartiles for each element (making use of "..." passing):
eapply(env, quantile, probs = 1:3/4)
eapply(env, quantile)
}
\keyword{iteration}
\keyword{environment}
\keyword{list}
