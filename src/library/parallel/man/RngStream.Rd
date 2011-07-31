% File src/library/parallel/man/RngStream.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{RNGstreams}
\alias{nextRNGStream}
\alias{nextRNGSubStream}
\title{Implementation of Pierre L'Ecuyer's RngStreams}
\description{
  This is an \R re-implementation of Pierre L'Ecuyer's \sQuote{RngStreams}
  multiple streams of (pseudo-)random numbers.
}
\usage{
nextRNGStream(seed)
nextRNGSubStream(seed)
}
\arguments{
  \item{seed}{An integer vector of length 7: see \code{\link{RNG}} for
    the valid values.}
}
\details{
  The \sQuote{RngStream} interface works with (potentially) multiple
  streams of pseudo-random numbers: this is particularly suitable for
  working with parallel computations since each task can be assigned a
  separate RNG stream.

  This uses as its underlying generator \code{RNGkind("L'Ecuyer-CMRG")},
  of L'Ecuyer (1999) which has a seed vector of 6 (signed) integers and a
  period of around \eqn{2^{191}}{2^191}.  Each \sQuote{stream} is a
  subsequence of the period of length \eqn{2^{127}}{2^127} which is in
  turn divided into \sQuote{substreams} of length \eqn{2^{76}}{2^76}.
}
\value{
  A value which can be assigned to \code{.Random.seed}.
}
\references{
  L’Ecuyer, P. (1999) Good parameters and implementations for combined
  multiple recursive random number generators. \emph{Operations
  Research} \bold{47}, 159--164.
  
  L'Ecuyer, P., Simard, R.,  Chen, E. J. and Kelton, W. D. (2002)
  An object-oriented random-number package with many long streams
  and substreams. \emph{Operations Research} \bold{50} 1073--5.
}
\seealso{
  \code{\link{RNG}} for \R's built-in random number generators.
}
\note{
  An interface to L'Ecuyer's C cade is available in CRAN
  package \pkg{rlecuyer}.
}

\examples{
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
(s <- .Random.seed)
nextRNGStream(s)
nextRNGSubStream(s)
}
\keyword{distribution}
\keyword{sysdata}
