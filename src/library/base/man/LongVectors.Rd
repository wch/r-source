% File src/library/base/man/Constants.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2012-2014 R Core Team
% Distributed under GPL 2 or later

\name{LongVectors}
\alias{long vector}
\alias{long vectors}
\alias{Long vectors}
\title{Long Vectors}
\description{
  Vectors of \eqn{2^{31}}{2^31} or more elements were added in \R 3.0.0.
}
\details{
  Prior to \R 3.0.0, all vectors in \R were restricted to at most
  \eqn{2^{31} - 1}{2^31 - 1} elements and could be indexed by integer
  vectors.

  Currently all \link{atomic} (raw, logical, integer, numeric, complex,
  character) vectors, \link{list}s and \link{expression}s can be much
  longer on 64-bit platforms: such vectors are referred to as
  \sQuote{long vectors} and have a slightly different internal
  structure.  In theory up they can to \eqn{2^{52}}{2^52} elements, but
  address space limits of current CPUs and OSes will be much smaller.
  Such objects will have a \link{length} that is expressed as a double,
  and can be indexed by double vectors.

  Arrays (including matrices) can be based on long vectors provided each
  of their dimensions is at most \eqn{2^{31} - 1}{2^31 - 1}: thus there
  are no 1-dimensional long arrays.

  \R code typically only needs minor changes to work with long vectors,
  maybe only checking that \code{as.integer} is not used unnecessarily
  for e.g.\sspace{}lengths.  However, compiled code typically needs quite
  extensive changes.  Note that the \code{\link{.C}} and
  \code{\link{.Fortran}} interfaces do not accept long vectors, so
  \code{\link{.Call}} (or similar) has to be used.

  Because of the storage requirements (a minimum of 64 bytes per
  character string), character vectors are only going to be usable if
  they have a small number of distinct elements, and even then factors
  will be more efficient (4 bytes per element rather than 8).  So it is
  expected that most of the usage of long vectors will be integer
  vectors (including factors) and numeric vectors.
}

\section{Matrix algebra}{
  It is now possible to use \eqn{m \times n}{m x n} matrices with more
  than 2 billion elements.  Whether matrix algebra (including
  \code{\link{\%*\%}}, \code{\link{crossprod}}, \code{\link{svd}},
  \code{\link{qr}}, \code{\link{solve}} and \code{\link{eigen}} will
  actually work is somewhat implementation dependent, including the
  Fortran compiler used and if an external BLAS or LAPACK is used.

  An efficient parallel BLAS implementation will often be important to
  obtain usable performance.  For example on one particular platform
  \code{chol} on a 47,000 square matrix took about 5 hours with the
  internal BLAS, 21 minutes using an optimized BLAS on one core, and 2
  minutes using an optimized BLAS on 16 cores.
}
