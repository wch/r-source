% File src/library/base/man/sample.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{sample}
\alias{sample}
\alias{sample.int}
\title{Random Samples and Permutations}
\description{
  \code{sample} takes a sample of the specified size from the elements
  of \code{x} using either with or without replacement.
}
\usage{
sample(x, size, replace = FALSE, prob = NULL)

sample.int(n, size, replace = FALSE, prob = NULL)
}
\arguments{
  \item{x}{Either a (numeric, complex, character or logical) vector of
    more than one element from which to choose, or a positive integer.}
  \item{n}{a non-negative integer, the number of items to choose from.}
  \item{size}{positive integer giving the number of items to choose.}
  \item{replace}{Should sampling be with replacement?}
  \item{prob}{A vector of probability weights for obtaining the elements
    of the vector being sampled.}
}
\details{
  If \code{x} has length 1, is numeric (in the sense of
  \code{\link{is.numeric}}) and \code{x >= 1}, sampling \emph{via}
  \code{sample} takes place from
  \code{1:x}.  \emph{Note} that this convenience feature may lead to
  undesired behaviour when \code{x} is of varying length
  \code{sample(x)}.  See the \code{resample()} example below.

  By default \code{size} is equal to \code{length(x)}
  so that \code{sample(x)} generates a random permutation
  of the elements of \code{x} (or \code{1:x}).

  The optional \code{prob} argument can be used to give a vector
  of weights for obtaining the elements of the vector being
  sampled. They need not sum to one, but they should be nonnegative
  and not all zero.  If \code{replace} is true, Walker's alias method
  (Ripley, 1987) is used when there are more than 250 reasonably
  probable values: this gives results incompatible with those from \R <
  2.2.0, and there will be a warning the first time this happens in a
  session.

  If \code{replace} is false, these probabilities are applied
  sequentially, that is the probability of choosing the next item is
  proportional to the weights amongst the remaining items.  The number
  of nonzero weights must be at least \code{size} in this case.
  
  \code{sample.int} is a bare interface in which both \code{n} and
  \code{size} must be supplied as integers.
}

\value{
  For \code{sample} a vector of length \code{size} with elements
  drawn from either \code{x} or from \code{1:x}.

  For \code{sample.int}, an integer vector of length \code{size} with
  elements from \code{1:n},
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Ripley, B. D. (1987) \emph{Stochastic Simulation}. Wiley.
}
\seealso{
  Package \pkg{sampling} for other methods of weighted sampling without
  replacement.
}
\examples{
x <- 1:12
# a random permutation
sample(x)
# bootstrap sampling -- only if length(x) > 1 !
sample(x,replace=TRUE)

# 100 Bernoulli trials
sample(c(0,1), 100, replace = TRUE)

## More careful bootstrapping --  Consider this when using sample()
## programmatically (i.e., in your function or simulation)!

# sample()'s surprise -- example
x <- 1:10
    sample(x[x >  8]) # length 2
    sample(x[x >  9]) # oops -- length 10!
try(sample(x[x > 10]))# error!

## This is safer, but only for sampling without replacement
resample <- function(x, size, ...)
  if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
  } else sample(x, size, ...)

resample(x[x >  8])# length 2
resample(x[x >  9])# length 1
resample(x[x > 10])# length 0
}
\keyword{distribution}
