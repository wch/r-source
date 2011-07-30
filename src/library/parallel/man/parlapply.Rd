% File src/library/parallel/man/parlapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{parlapply}
\alias{parlapply}
\title{
  A Parallel Version of \code{lapply}
}
\description{
  A parallel version of \code{\link{lapply}}.
}
\usage{
parlapply(X, FUN, ..., Ncpus)
}
\arguments{
  \item{X}{a vector (atomic or list) or an \code{\link{expression}}
    object.  Other objects (including classed objects) will be coerced
    by \code{base::\link{as.list}}.}
  \item{FUN}{the function to be applied to each element of \code{X}:
    see \sQuote{Details}.  In the case of functions like
    \code{+}, \code{\%*\%}, the function name must be backquoted or quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{Ncpus}{The number of CPUs to use.}
}
\details{
  More to come
}
\value{
  A list of the same length as \code{X}.
}
\keyword{iteration}
