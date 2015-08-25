% File src/library/parallel/man/unix/pvec.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2014 R Core Team
% Distributed under GPL 2 or later

\name{pvec}
\alias{pvec}
\title{Parallelize a Vector Map Function using Forking}
\description{
  \code{pvec} parellelizes the execution of a function on vector elements
  by splitting the vector and submitting each part to one core. The
  function must be a vectorized map, i.e.\sspace{}it takes a vector input and
  creates a vector output of exactly the same length as the input which
  doesn't depend on the partition of the vector.

  It relies on forking and hence is not available on Windows unless
  \code{mc.cores = 1}.
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
  \item{mc.silent}{if set to \code{TRUE} then all output on \file{stdout} will
    be suppressed for all parallel processes forked (\file{stderr} is not
    affected).}
  \item{mc.cores}{The number of cores to use, i.e.\sspace{}at most how many
    child processes will be run simultaneously.  Must be at least one,
    and at least two for parallel operation.  The option is initialized
    from environment variable \env{MC_CORES} if set.}
  \item{mc.cleanup}{See the description of this argument in
    \code{\link{mclapply}}.}
}

\details{
  \code{pvec} parallelizes \code{FUN(x, ...)} where \code{FUN} is a
  function that returns a vector of the same length as
  \code{x}. \code{FUN} must also be pure (i.e., without side-effects)
  since side-effects are not collected from the parallel processes. The
  vector is split into nearly identically sized subvectors on which
  \code{FUN} is run.  Although it is in principle possible to use
  functions that are not necessarily maps, the interpretation would be
  case-specific as the splitting is in theory arbitrary (a warning is
  given in such cases).

  The major difference between \code{pvec} and \code{\link{mclapply}} is
  that \code{mclapply} will run \code{FUN} on each element separately
  whereas \code{pvec} assumes that \code{c(FUN(x[1]), FUN(x[2]))} is
  equivalent to \code{FUN(x[1:2])} and thus will split into as many
  calls to \code{FUN} as there are cores (or elements, if fewer), each
  handling a subset vector.  This makes it more efficient than
  \code{mclapply} but requires the above assumption on \code{FUN}.

  If \code{mc.cores == 1} this evaluates \code{FUN(v, ...)} in the
  current process.
}

\value{
  The result of the computation -- in a successful case it should be of
  the same length as \code{v}.  If an error occurred or the function was
  not a map the result may be shorter or longer, and a warning is given.
}

\note{
  Due to the nature of the parallelization, error handling does not
  follow the usual rules since errors will be returned as strings and
  results from killed child processes will show up simply as
  non-existent data.  Therefore it is the responsibility of the user to
  check the length of the result to make sure it is of the correct size.
  \code{pvec} raises a warning if that is the case since it does not
  know whether such an outcome is intentional or not.

  See \code{\link{mcfork}} for the inadvisability of using this with
  GUI front-ends.
}

\author{
  Simon Urbanek and R Core.

  Derived from the \CRANpkg{multicore} package.
}
\seealso{
  \code{\link{mcparallel}}, \code{\link{mclapply}},
  \code{\link{parLapply}}, \code{\link{clusterMap}}.
}
\examples{
x <- pvec(1:1000, sqrt)
stopifnot(all(x == sqrt(1:1000)))

\donttest{
# One use is to convert date strings to unix time in large datasets
# as that is a relatively slow operation.
# So let's get some random dates first
# (A small test only with 2 cores: set options("mc.cores")
# and increase N for a larger-scale test.)
N <- 1e5
dates <- sprintf('\%04d-\%02d-\%02d', as.integer(2000+rnorm(N)),
                 as.integer(runif(N, 1, 12)), as.integer(runif(N, 1, 28)))

system.time(a <- as.POSIXct(dates))

# But specifying the format is faster
system.time(a <- as.POSIXct(dates, format = "\%Y-\%m-\%d"))

# pvec ought to be faster, but system overhead can be high
system.time(b <- pvec(dates, as.POSIXct, format = "\%Y-\%m-\%d"))
stopifnot(all(a == b))

# using mclapply for this would much slower because each value
# will require a separate call to as.POSIXct()
# as lapply(dates, as.POSIXct) does
system.time(c <- unlist(mclapply(dates, as.POSIXct,  format = "\%Y-\%m-\%d")))
stopifnot(all(a == c))
}}
\keyword{interface}
