\name{findInt}
\alias{findInt}
\title{Find Interval Numbers or Indices}
\usage{
findInt(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
}
\arguments{
  \item{x}{numeric.}
  \item{vec}{numeric, sorted (weakly) increasingly, of length \code{N}, say.}
  \item{rightmost.closed}{logical; if true, the rightmost interval,
    \code{vec[N-1] .. vec[N]} is treated as \emph{closed}, see below.}
  \item{all.inside}{logical; if true, the returned indices are coerced
    into \eqn{\{1,\dots,N-1\}}, i.e. 0 is mapped to 1 and N to N-1.}
}
\description{
  Find the indices of \code{x} in \code{vec}, where \code{vec} must be
  sorted (non-decreasingly); i.e., if \code{i <- findInt(x,v)},
  we have \eqn{v_{i_j} \le x_j < v_{i_j + 1}}{v[i[j]] <= x[j] < v[i[j] + 1]}
  where \eqn{v_0 := -\infty}{v[0] := - Inf},
  \eqn{v_{N+1} := +\infty}{v[N+1] := + Inf}, and \code{N <- length(vec)}.
  At the two boundaries, the returned index may differ by 1, depending
  on the optional arguments \code{rightmost.closed} and \code{all.inside}.
}
\details{
  The function \code{findInt} finds the index of one vector \code{x} in
  another, \code{vec}, where the latter must be non-decreasing.  Where
  this is trivial, equivalent to \code{apply( outer(x, vec, ">="), 1, sum)},
  as a matter of fact, the internal algorithm uses interval search
  ensuring \eqn{O(n \log N)}{O(n * log(N))} complexity where
  \code{n <- length(x)} (and \code{N <- length(vec)}).  For (almost)
  sorted \code{x}, it will be even faster, basically \eqn{O(n)}.

  This is the same computation as for the empirical distribution
  function, and indeed, \code{findInt(t, sort(X))} is
  \emph{identical} to \eqn{n F_n(t; X_1,\dots,X_n)}{n * Fn(t;
    X[1],..,X[n])} where \eqn{F_n}{Fn} is the empirical distribution
  function of \eqn{X_1,\dots,X_n}{X[1],..,X[n]}.

  When \code{rightmost.closed = TRUE}, the result
  for \code{x[j] = vec[N]} (\eqn{ = \max(vec)}), is \code{N - 1} as for
  all other values in the last interval.
}
\value{
  vector of length \code{length(x)} with values in \code{0:N} where
  \code{N <- length(vec)}, or values coerced to \code{1:(N-1)} iff
  \code{all.inside = TRUE} (equivalently coercing all x values
  \emph{inside} the intervals).
}
\author{Martin Maechler}
\seealso{\code{\link{approx}(*, method = "constant")} which is a
  generalization of \code{findInt()}, \code{\link[stepfun]{ecdf}} for
  computing the empirical distribution function which is (up to a factor
  of \eqn{n}) also basically the same as findInt(.).
}
\examples{
N <- 100
X <- sort(round(rt(N, df=2), 2))
tt <- c(-100, seq(-2,2, len=201), +100)
it <- findInt(tt, X)
tt[it < 1 | it >= N] # only first and last are outside range(X)

## See that this is N * Fn(.) :
tt <- c(tt,X)
eps <- 100 * .Machine$double.eps
require(stepfun)
stopifnot( it[c(1,203)] == c(0, 100),
          all.equal(N * ecdf(X)(tt),
                    findInt(tt, X),  tol = eps),
          findInt(tt,X) ==  apply( outer(tt, X, ">="), 1, sum)
 )
\testonly{
 ## CPU comparisons
 set.seed(91)
 n <- 200; nc <- 100
 cat("CPU used -- n = ",n,"; for(i in 1:",nc,") ... (in 0.01 sec) : \n",
     "   N  | findInt() |  N * Fn() | apply(outer(*,*, >=),1,sum)\n",
     "------|-----------|-----------|----------------------------\n",sep="")
##    1 3 5 7 9 1 3 5 7 9 1 3
 mform <- function(x) formatC(100 * x[1], wid = 10, digits = 0, format = "f")
 tt <- ind <- in2 <- matrix(NA, n,nc)
 for(N in as.integer(1000* 2^(-1:4))) {
   X <- sort(rt(N, df =3))
   Fn <- ecdf(X)
   for(i in 1:nc) tt[,i] <- rt(n, df=1)
   cat(formatC(N, wid=5),
       mform(system.time(for(i in 1:nc) ind[,i] <- findInt(tt[,i], X))),
       mform(system.time(for(i in 1:nc) in2[,i] <- N * Fn (tt[,i]))),
       ## only one instead of nc ( = 50 ) times -- and still slower!:
       mform(nc*system.time(in1 <- apply(outer(tt[,1], X, ">="), 1, sum))),
       sep=" |", "\n")
   in2 <- round(in2) ; storage.mode(in2) <- "integer"
   stopifnot(identical(ind,     in2),
             identical(ind[,1], as.integer(round(in1))))
 }# for
}%testonly
}
\keyword{arith}
\keyword{utilities}
