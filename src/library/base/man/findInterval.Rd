% File src/library/base/man/findInterval.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{findInterval}
\alias{findInterval}
\title{Find Interval Numbers or Indices}
\usage{
findInterval(x, vec, rightmost.closed = FALSE, all.inside = FALSE,
             left.open = FALSE)
}
\arguments{
  \item{x}{numeric.}
  \item{vec}{numeric, sorted (weakly) increasingly, of length \code{N},
    say.}
  \item{rightmost.closed}{logical; if true, the rightmost interval,
    \code{vec[N-1] .. vec[N]} is treated as \emph{closed}, see below.}
  \item{all.inside}{logical; if true, the returned indices are coerced
    into \code{1,\dots,N-1}, i.e., \code{0} is mapped to \code{1}
    and \code{N} to \code{N-1}.}
  \item{left.open}{logical; if true all the intervals are open at left
    and closed at right; in the formulas below, \eqn{\le} should be
    swapped with \eqn{<} (and \eqn{>} with \eqn{\ge}), and
    \code{rightmost.closed} means \sQuote{leftmost is closed}.  This may
    be useful, e.g., in survival analysis computations.}
}
\description{
  Given a vector of non-decreasing breakpoints in \code{vec}, find the
  interval containing each element of \code{x}; i.e., if
  \code{i <- findInterval(x,v)}, for each index \code{j} in \code{x}
  \eqn{v_{i_j} \le x_j < v_{i_j + 1}}{v[i[j]] \le x[j] < v[i[j] + 1]}
  where \eqn{v_0 := -\infty}{v[0] := - Inf},
  \eqn{v_{N+1} := +\infty}{v[N+1] := + Inf}, and \code{N <- length(v)}.
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

  When \code{rightmost.closed = TRUE}, the result for \code{x[j] = vec[N]}
  (\eqn{ = \max vec}{ = max(vec)}), is \code{N - 1} as for all other
  values in the last interval.

  \code{left.open = TRUE} is occasionally useful, e.g., for survival data.
  For (anti-)symmetry reasons, it is equivalent to using
  \dQuote{mirrored} data, i.e., the following is always true:
  \preformatted{
    identical(
          findInterval( x,  v,      left.open= TRUE, ...) ,
      N - findInterval(-x, -v[N:1], left.open=FALSE, ...) )
  }
  where \code{N <- length(vec)} as above.
}
\value{
  vector of length \code{length(x)} with values in \code{0:N} (and
  \code{NA}) where \code{N <- length(vec)}, or values coerced to
  \code{1:(N-1)} if and only if \code{all.inside = TRUE} (equivalently coercing all
  x values \emph{inside} the intervals).  Note that \code{\link{NA}}s are
  propagated from \code{x}, and \code{\link{Inf}} values are allowed in
  both \code{x} and \code{vec}.
}
\author{Martin Maechler}
\seealso{\code{\link{approx}(*, method = "constant")} which is a
  generalization of \code{findInterval()}, \code{\link{ecdf}} for
  computing the empirical distribution function which is (up to a factor
  of \eqn{n}) also basically the same as \code{findInterval(.)}.
}
\examples{
x <- 2:18
v <- c(5, 10, 15) # create two bins [5,10) and [10,15)
cbind(x, findInterval(x, v))

N <- 100
X <- sort(round(stats::rt(N, df = 2), 2))
tt <- c(-100, seq(-2, 2, len = 201), +100)
it <- findInterval(tt, X)
tt[it < 1 | it >= N] # only first and last are outside range(X)

##  'left.open = TRUE' means  "mirroring" :
N <- length(v)
stopifnot(identical(
                  findInterval( x,  v,  left.open=TRUE) ,
              N - findInterval(-x, -v[N:1])))
}
\keyword{arith}
\keyword{utilities}
