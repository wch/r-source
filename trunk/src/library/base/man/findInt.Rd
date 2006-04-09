\name{findInterval}
\alias{findInterval}
\title{Find Interval Numbers or Indices}
\usage{
findInterval(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
}
\arguments{
  \item{x}{numeric.}
  \item{vec}{numeric, sorted (weakly) increasingly, of length \code{N},
    say.}
  \item{rightmost.closed}{logical; if true, the rightmost interval,
    \code{vec[N-1] .. vec[N]} is treated as \emph{closed}, see below.}
  \item{all.inside}{logical; if true, the returned indices are coerced
    into \eqn{\{1,\dots,N-1\}}, i.e., 0 is mapped to 1 and \eqn{N} to
    \eqn{N-1}.}
}
\description{
  Find the indices of \code{x} in \code{vec}, where \code{vec} must be
  sorted (non-decreasingly); i.e., if \code{i <- findInterval(x,v)},
  we have \eqn{v_{i_j} \le x_j < v_{i_j + 1}}{v[i[j]] <= x[j] < v[i[j] + 1]}
  where \eqn{v_0 := -\infty}{v[0] := - Inf},
  \eqn{v_{N+1} := +\infty}{v[N+1] := + Inf}, and \code{N <- length(vec)}.
  At the two boundaries, the returned index may differ by 1, depending
  on the optional arguments \code{rightmost.closed} and \code{all.inside}.
}
\details{
  The function \code{findInterval} finds the index of one vector \code{x} in
  another, \code{vec}, where the latter must be non-decreasing.  Where
  this is trivial, equivalent to \code{apply( outer(x, vec, ">="), 1, sum)},
  as a matter of fact, the internal algorithm uses interval search
  ensuring \eqn{O(n \log N)}{O(n * log(N))} complexity where
  \code{n <- length(x)} (and \code{N <- length(vec)}).  For (almost)
  sorted \code{x}, it will be even faster, basically \eqn{O(n)}.

  This is the same computation as for the empirical distribution
  function, and indeed, \code{findInterval(t, sort(X))} is
  \emph{identical} to \eqn{n F_n(t; X_1,\dots,X_n)}{n * Fn(t;
    X[1],..,X[n])} where \eqn{F_n}{Fn} is the empirical distribution
  function of \eqn{X_1,\dots,X_n}{X[1],..,X[n]}.

  When \code{rightmost.closed = TRUE}, the result
  for \code{x[j] = vec[N]} (\eqn{ = \max(vec)}), is \code{N - 1} as for
  all other values in the last interval.
}
\value{
  vector of length \code{length(x)} with values in \code{0:N} (and
  \code{NA}) where \code{N <- length(vec)}, or values coerced to
  \code{1:(N-1)} iff \code{all.inside = TRUE} (equivalently coercing all
  x values \emph{inside} the intervals).  Note that \code{\link{NA}}s are
  propagated from \code{x}, and \code{\link{Inf}} values are allowed in
  both \code{x} and \code{vec}.
}
\author{Martin Maechler}
\seealso{\code{\link{approx}(*, method = "constant")} which is a
  generalization of \code{findInterval()}, \code{\link{ecdf}} for
  computing the empirical distribution function which is (up to a factor
  of \eqn{n}) also basically the same as findInterval(.).
}
\examples{
N <- 100
X <- sort(round(rt(N, df=2), 2))
tt <- c(-100, seq(-2,2, len=201), +100)
it <- findInterval(tt, X)
tt[it < 1 | it >= N] # only first and last are outside range(X)
}
\keyword{arith}
\keyword{utilities}
