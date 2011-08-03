% File src/library/parallel/man/pvec.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Development Team
% Distributed under GPL 2 or later

\name{pvec}
\alias{pvec}
\title{Parallelize a Vector Map Function}
\description{
  \code{pvec} parellelizes the execution of a function on vector elements
  by splitting the vector and submitting each part to one core. The
  function must be a vectorized map, i.e. it takes a vector input and
  creates a vector output of exactly the same length as the input which
  doesn't depend on the partition of the vector.
}
\usage{
pvec(v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE,
     mc.cores = getOption("mc.cores", 2L), mc.cleanup = TRUE)
}
\arguments{
  \item{v}{vector to operate on}
  \item{FUN}{function to call on each part of the vector}
  \item{\dots}{any further arguments passed to \code{FUN} after the vector}
  \item{mc.set.seed}{See \code{\link{mcparallel}}.}
  \item{mc.silent}{if set to \code{TRUE} then all output on stdout will
    be suppressed for all parallel processes forked (stderr is not
    affected).}
  \item{mc.cores}{The number of cores to use, i.e. how many processes
    will be forked (at most)}
  \item{mc.cleanup}{flag specifying whether pvec should be
    terminated when the master is aborted (see description of this
    argument in \code{\link{mclapply}} for details)}
}
\details{
  \code{pvec} parallelizes \code{FUN(x, ...)} where \code{FUN} is a
  function that returns a vector of the same length as
  \code{x}. \code{FUN} must also be pure (i.e., without side-effects)
  since side-effects are not collected from the parallel processes. The
  vector is split into nearly identically sized subvectors on which
  \code{FUN} is run. Although it is in principle possible to use
  functions that are not necessarily maps, the interpretation would be
  case-specific as the splitting is in theory arbitrary and a warning is
  given in such cases.

  The major difference between \code{pvec} and \code{\link{mclapply}} is
  that \code{mclapply} will run \code{FUN} on each element separately
  whereas \code{pvec} assumes that \code{c(FUN(x[1]), FUN(x[2]))} is
  equivalent to \code{FUN(x[1:2])} and thus will split into as many
  calls to \code{FUN} as there are cores, each handling a subset
  vector. This makes it much more efficient than \code{mclapply} but
  requires the above assumption on \code{FUN}.
}
\value{
  The result of the computation - in a successful case it should be of
  the same length as \code{v}.  If an error ocurred or the function was
  not a map the result may be shorter and a warning is given.
}
\note{
  Due to the nature of the parallelization, error handling does not
  follow the usual rules since errors will be returned as strings and
  killed child processes will show up simply as non-existent
  data.  Therefore it is the responsibiliy of the user to check the
  length of the result to make sure it is of the correct
  size.  \code{pvec} raises a warning if that is the case since it does
  not know whether such outcome is intentional or not.
}
\seealso{
  \code{\link{mcparallel}}, \code{\link{mclapply}}
}
\examples{
x <- pvec(1:1000, sqrt)
stopifnot(all(x == sqrt(1:1000)))

# a use is to convert dates to unix time in large datasets
# as that is an awfully slow operation
# so let's get some random dates first (a small test only)
N <- 1e4; nc <- 2L
dates <- sprintf('\%04d-\%02d-\%02d', as.integer(2000+rnorm(N)),
                 as.integer(runif(N, 1, 12)), as.integer(runif(N, 1, 28)))

system.time(a <- as.POSIXct(dates))
system.time(b <- pvec(dates, as.POSIXct, cores = nc))

stopifnot(all(a == b))

# using mclapply for this is much slower because each value
# will require a separate call to as.POSIXct(), like lapply(dates, as.POSIXct)
system.time(c <- unlist(mclapply(dates, as.POSIXct, cores = nc)))
}
\keyword{interface}
