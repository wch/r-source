\name{sample}
\title{Random Samples and Permutations}
\usage{
sample(x, size, replace=FALSE, prob)
}
\alias{sample}
\arguments{
  \item{x}{Either a (numeric, complex, character or logical)
    vector of more than one element from which to
    choose, or a positive integer.}
  \item{size}{A positive integer giving the number of items to choose.}
  \item{replace}{Should sampling be with replacement?}
  \item{prob}{A vector of probabilities of obtaining the elements
    of the vector being sampled.}
}
\description{
  \code{sample} takes a sample of the specified size from
  the elements of \code{x} using either with or without replacement.
}
\details{
  If \code{x} has length 1, sampling takes place from
  \code{1:x}.

  By default \code{size} is equal to \code{length(x)}
  so that \code{sample(x)} generates a random permutation
  of the elements of \code{x} (or \code{1:x}).
  
  The optional \code{prob} argument can be used to give a vector
  of probabilities of obtaining the elements of the vector being
  sampled. If \code{replace} is false, these probabilities are applied
  sequentially, that is the probability of choosing the next item is
  proportional to the probabilities amongst the remaining items.
}
\examples{
x <- 1:12
# a random permutation
sample(x)
# bootstrap sampling
sample(x,replace=TRUE)

# 100 Bernoulli trials
sample(c(0,1), 100, replace = TRUE)
}
\keyword{distribution}
