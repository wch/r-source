\name{sample}
\title{Random Samples and Permutations}
\usage{
sample(x, size, replace=FALSE, prob)
}
\alias{sample}
\description{
\code{sample} takes a sample of the specified size from
the elements of \code{x} using either with or without replacement
sampling according to the value of \code{replace}.

By default \code{size} is equal to \code{length(x)}
so that \code{sample(x)} generates a random permutation
of the elements of \code{x}.

If \code{x} has length 1, sampling takes place from
\code{1:x}.

The optional \code{prob} agrument can be used to give a vector
of probabilities of obtaining the elements of the vector being
sampled.
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
