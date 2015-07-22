% File src/library/parallel/man/RngStream.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Team
% Distributed under GPL 2 or later

\name{RNGstreams}
\alias{nextRNGStream}
\alias{nextRNGSubStream}
\alias{clusterSetRNGStream}
\alias{mc.reset.stream}

\title{Implementation of Pierre L'Ecuyer's RngStreams}
\description{
  This is an \R re-implementation of Pierre L'Ecuyer's \sQuote{RngStreams}
  multiple streams of pseudo-random numbers.
}
\usage{
nextRNGStream(seed)
nextRNGSubStream(seed)

clusterSetRNGStream(cl = NULL, iseed)
mc.reset.stream()
}
\arguments{
  \item{seed}{An integer vector of length 7 as given by
    \code{.Random.seed} when the \samp{"L'Ecuyer-CMRG"} RNG is in use.
    See \code{\link{RNG}} for the valid values.}
  \item{cl}{A cluster from this package or package \CRANpkg{snow}, or (if
    \code{NULL}) the registered cluster.}
  \item{iseed}{An integer to be supplied to \code{\link{set.seed}}, or
    \code{NULL} not to set reproducible seeds.}
}
\details{
  The \sQuote{RngStream} interface works with (potentially) multiple
  streams of pseudo-random numbers: this is particularly suitable for
  working with parallel computations since each task can be assigned a
  separate RNG stream.

  This uses as its underlying generator \code{RNGkind("L'Ecuyer-CMRG")},
  of L'Ecuyer (1999), which has a seed vector of 6 (signed) integers and a
  period of around \eqn{2^{191}}{2^191}.  Each \sQuote{stream} is a
  subsequence of the period of length \eqn{2^{127}}{2^127} which is in
  turn divided into \sQuote{substreams} of length \eqn{2^{76}}{2^76}.

  The idea of L'Ecuyer \emph{et al} (2002) is to use a separate stream
  for each of the parallel computations (which ensures that the random
  numbers generated never get into to sync) and the parallel
  computations can themselves use substreams if required.  The original
  interface stores the original seed of the first stream, the original
  seed of the current stream and the current seed: this could be
  implemented in \R, but it is as easy to work by saving the relevant
  values of \code{.Random.seed}: see the examples.

  \code{clusterSetRNGStream} selects the \code{"L'Ecuyer-CMRG"} RNG and
  then distributes streams to the members of a cluster, optionally
  setting the seed of the streams by \code{set.seed(iseed)} (otherwise
  they are set from the current seed of the master process: after
  selecting the L'Ecuyer generator).
#ifdef unix

  Calling \code{mc.reset.stream()} after setting the L'Ecuyer random
  number generator and seed makes runs from
  \code{\link{mcparallel}(mc.set.seed = TRUE)} reproducible.  This is
  done internally in \code{\link{mclapply}} and \code{\link{pvec}}.
  (Note that it does not set the seed in the master process, so does not
  affect the fallback-to-serial versions of these functions.)
#endif
}
\value{
  For \code{nextRNGStream} and \code{nextRNGSubStream},
  a value which can be assigned to \code{.Random.seed}.
}
\references{
  L'Ecuyer, P. (1999) Good parameters and implementations for combined
  multiple recursive random number generators.  \emph{Operations
  Research} \bold{47}, 159--164.

  L'Ecuyer, P., Simard, R.,  Chen, E. J. and Kelton, W. D. (2002)
  An object-oriented random-number package with many long streams
  and substreams.  \emph{Operations Research} \bold{50} 1073--5.
}
\author{
  Brian Ripley
}
\seealso{
  \code{\link{RNG}} for fuller details of \R's built-in random number
  generators.

  The vignette for package \pkg{parallel}.
}
\note{
  Interfaces to L'Ecuyer's C code are available in CRAN
  packages \CRANpkg{rlecuyer} and \CRANpkg{rstream}.
}

\examples{
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
(s <- .Random.seed)
## do some work involving random numbers.
nextRNGStream(s)
nextRNGSubStream(s)
}
\keyword{distribution}
\keyword{sysdata}
