% File src/library/stats/man/Multinomal.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Multinom}
\alias{Multinomial}
\alias{rmultinom}
\alias{dmultinom}
\title{The Multinomial Distribution}
\description{
  Generate multinomially distributed random number vectors and
  compute multinomial probabilities.
}
\usage{
rmultinom(n, size, prob)
dmultinom(x, size = NULL, prob, log = FALSE)
}
\arguments{
  \item{x}{vector of length \eqn{K} of integers in \code{0:size}.}
  %%FUTURE:  matrix of \eqn{K} rows or ...
  \item{n}{number of random vectors to draw.}
  \item{size}{integer, say \eqn{N}, specifying the total number
    of objects that are put into \eqn{K} boxes in the typical multinomial
    experiment. For \code{dmultinom}, it defaults to \code{sum(x)}.}
  \item{prob}{numeric non-negative vector of length \eqn{K}, specifying
    the probability for the \eqn{K} classes; is internally normalized to
    sum 1.}
  \item{log}{logical; if TRUE, log probabilities are computed.}
}
\note{\code{dmultinom} is currently \emph{not vectorized} at all and has
  no C interface (API); this may be amended in the future.% yes, DO THIS!
}
\details{
  If \code{x} is a \eqn{K}-component vector, \code{dmultinom(x, prob)}
  is the probability
  \deqn{P(X_1=x_1,\ldots,X_K=x_k) = C \times \prod_{j=1}^K
    \pi_j^{x_j}}{P(X[1]=x[1], \dots , X[K]=x[k]) = C * prod(j=1 , \dots, K) p[j]^x[j]}
  where \eqn{C} is the \sQuote{multinomial coefficient}
  \eqn{C = N! / (x_1! \cdots x_K!)}{C = N! / (x[1]! * \dots * x[K]!)}
  and \eqn{N = \sum_{j=1}^K x_j}{N = sum(j=1, \dots, K) x[j]}.
  \cr
  By definition, each component \eqn{X_j}{X[j]} is binomially distributed as
  \code{Bin(size, prob[j])} for \eqn{j = 1, \ldots, K}.

  The \code{rmultinom()} algorithm draws binomials \eqn{X_j}{X[j]} from
  \eqn{Bin(n_j,P_j)}{Bin(n[j], P[j])} sequentially, where
  \eqn{n_1 = N}{n[1] = N} (N := \code{size}),
  \eqn{P_1 = \pi_1}{P[1] = p[1]} (\eqn{\pi}{p} is \code{prob} scaled to sum 1),
  and for \eqn{j \ge 2}, recursively,
  \eqn{n_j = N - \sum_{k=1}^{j-1} X_k}{n[j] = N - sum(k=1, \dots, j-1) X[k]}
  and
  \eqn{P_j = \pi_j / (1 - \sum_{k=1}^{j-1} \pi_k)}{P[j] = p[j] / (1 - sum(p[1:(j-1)]))}.
}
\value{
  For \code{rmultinom()},
  an integer \eqn{K \times n}{K x n} matrix where each column is a
  random vector generated according to the desired multinomial law, and
  hence summing to \code{size}.  Whereas the \emph{transposed} result
  would seem more natural at first, the returned matrix is more
  efficient because of columnwise storage.
}
\seealso{
  \link{Distributions} for standard distributions, including
  \code{\link{dbinom}} which is a special case conceptually.
%% but does not return 2-vectors
}
\examples{
rmultinom(10, size = 12, prob = c(0.1,0.2,0.8))

pr <- c(1,3,6,10) # normalization not necessary for generation
rmultinom(10, 20, prob = pr)

## all possible outcomes of Multinom(N = 3, K = 3)
X <- t(as.matrix(expand.grid(0:3, 0:3))); X <- X[, colSums(X) <= 3]
X <- rbind(X, 3:3 - colSums(X)); dimnames(X) <- list(letters[1:3], NULL)
X
round(apply(X, 2, function(x) dmultinom(x, prob = c(1,2,5))), 3)
}
\keyword{distribution}
