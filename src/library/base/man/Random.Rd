\name{Random}
\title{Random Number Generation}
\usage{
.Random.seed <- c(rng.kind, n1, n2, \dots)
save.seed <- .Random.seed

RNGkind(kind = NULL, normal.kind = NULL)
set.seed(seed, kind = NULL)
}
\alias{.Random.seed}
\alias{RNG}
\alias{RNGkind}
\alias{set.seed}
\description{
  \code{.Random.seed} is an integer vector, containing the random number
  generator (RNG) \bold{state} for random number generation in \R. It
  can be saved and restored, but should not be altered by the user.

  \code{RNGkind} is a more friendly interface to query or set the kind
  of RNG in use.

  \code{set.seed} is the recommended way to specify seeds.
}
\arguments{
  \item{kind}{character or \code{NULL}.	 If \code{kind} is a character
    string, set \R's RNG to the kind desired. If it is \code{NULL},
    return the currently used RNG.}
  \item{normal.kind}{character string or \code{NULL}.  If it is a character
    string, set the method of Normal generation.}
  \item{seed}{a single value, interpreted as an integer.}
  \item{rng.kind}{integer code in \code{0:k} for the above \code{kind}.}
  \item{n1, n2, \dots}{integers. See the details for how many are required
    (which depends on \code{rng.kind}).}
}
\details{
%% source and more detailed references:  ../../../main/RNG.c
  The currently available RNG kinds are given below.  \code{kind} is
  partially matched to this list.  The default is
  \code{"Marsaglia-Multicarry"}.
  \describe{
    \item{\code{"Wichmann-Hill"}}{
    The seed, \code{.Random.seed[-1] == r[1:3]} is an integer vector of
    length 3, where each \code{r[i]} is in \code{1:(p[i] - 1)}, where
    \code{p} is the length 3 vector of primes, \code{p = (30269, 30307,
      30323)}.
    The Wichmann--Hill generator has a cycle length of
    \eqn{6.9536 \times 10^{12}}{6.9536e12} (=
    \code{prod(p-1)/4}, see \emph{Applied Statistics} (1984)
    \bold{33}, 123 which corrects the original article).}

    \item{\code{"Marsaglia-Multicarry"}:}{
    A \emph{multiply-with-carry} RNG is used, as recommended by George
    Marsaglia in his post to the mailing list \file{sci.stat.math}.
    It has a period of more than \eqn{2^{60}}{2^60} and has passed
    all tests (according to Marsaglia).	 The seed is two integers (all
    values allowed).}

    \item{\code{"Super-Duper"}:}{
    Marsaglia's famous Super-Duper from the 70's.  This is the original
    version which does \emph{not} pass the MTUPLE test of the Diehard
    battery.  It has a period of \eqn{\approx 4.6\times 10^{18}}{about
      4.6*10^18} for most initial seeds.  The seed is two integers (all
    values allowed for the first seed: the second must be odd).

    We use the implementation by Reeds et al.\ (1982--84).

    The two seeds are the Tausworthe and congruence long integers,
    respectively.  A one-to-one mapping to S's \code{.Random.seed[1:12]}
    is possible but we will not publish one, not least as this generator
    is \bold{not} exactly the same as that in recent versions of S-PLUS.}

    \item{\code{"Mersenne-Twister":}}{
    From Matsumoto and Nishimura (1998). A twisted GFSR with period
    \eqn{2^{19937} - 1}{2^19937 - 1} and equidistribution in 623
    consecutive dimensions (over the whole period).  The ``seed'' is a
    624-dimensional set of 32-bit integers plus a current position in
    that set.}

    \item{\code{"Knuth-TAOCP":}}{
    From Knuth (1997).	A GFSR using lagged Fibonacci sequences with
    subtraction. That is, the recurrence used is
    \deqn{X_j = (X_{j-100} - X_{j-37}) \bmod 2^{30}%
        }{X[j] = (X[j-100] - X[j-37]) mod 2^30}
    and the ``seed'' is the set of the 100 last numbers (actually
    recorded as 101 numbers, the last being a cyclic shift of the
    buffer).  The period is around \eqn{2^{129}}{2^129}.}
  
    \item{\code{"user-supplied":}}{
    Use a user-supplied generator. See \code{\link{Random.user}} for details.
    }
  }

  \code{normal.kind} can be \code{"Kinderman-Ramage"} (the default) or
  \code{"Ahrens-Dieter"} or \code{"Box-Muller"}.

  \code{set.seed} uses its single integer argument to set as many seeds
  as are required.  It is intended as a simple way to get quite different
  seeds by specifying small integer arguments, and also as a way to get
  valid seed sets for the more complicated methods (especially
  \code{"Knuth-TAOCP"}).
}
\value{
  \code{.Random.seed} is an \code{\link{integer}} vector whose first
  element \emph{codes} the kind of RNG and normal generator. The lowest
  two decimal digits are in in \code{0:(k-1)}
  where \code{k} is the number of available RNGs. The thousands
  represent the type of normal generator (starting at \code{0}).

  In the underlying C, \code{.Random.seed[-1]} is \code{unsigned};
  therefore in \R \code{.Random.seed[-1]} can be negative.

  \code{RNGkind} returns a two-element character vector of the RNG and
  normal kinds in use \emph{before} the call, invisibly if either
  argument is not \code{NULL}.

  \code{set.seed} returns \code{NULL}, invisibly.
}
\references{
  Wichmann, B. A.  and Hill, I. D. (1982)
  \emph{Algorithm AS 183: An Efficient and Portable Pseudo-random Number
    Generator}, Applied Statistics, \bold{31}, 188--190; Remarks:
  \bold{34}, 198 and \bold{35}, 89.

  De Matteis, A. and Pagnutti, S. (1993)
  \emph{Long-range Correlation Analysis of the Wichmann-Hill Random
      Number Generator}, Statist. Comput., \bold{3}, 67--70.

  Marsaglia, G. (1997) \emph{A random number generator for C.} Discussion
  paper, posting on Usenet newsgroup \code{sci.stat.math} on
  September 29, 1997.

  Reeds, J., Hubert, S. and Abrahams, M. (1982--4) C implementation of
  SuperDuper, University of California at Berkeley.  (Personal
  communication from Jim Reeds to Ross Ihaka.)

  Marsaglia, G. and Zaman, A. (1994) Some portable very-long-period
  random number generators. \emph{Computers in Physics}, \bold{8},
  117--121.

  Matsumoto, M. and Nishimura, T. (1998)
  Mersenne Twister: A 623-dimensionally equidistributed uniform
  pseudo-random number generator,
  \emph{ACM Transactions on Modeling and Computer Simulation},
  \bold{8}, 3--30.\cr
  Source code at \url{http://www.math.keio.ac.jp/~matumoto/emt.html}.

  Knuth, D. E. (1997)
  \emph{The Art of Computer Programming.} Volume 2, third edition.\cr
  Source code at \url{http://www-cs-faculty.stanford.edu/~knuth/taocp.html}.
}
\note{
  Initially, there is no seed;	a new one is created from the current
  time when one is required.  Hence, different sessions will give
  different simulation results, by default.
}
\author{of RNGkind: Martin Maechler. Current implementation, B. D. Ripley}

\seealso{\code{\link{runif}}, \code{\link{rnorm}}, \ldots.}
%	this is ./Uniform.Rd
\examples{
runif(1); .Random.seed; runif(1); .Random.seed
## If there is no seed,	 a ``random'' new one is created:
rm(.Random.seed); runif(1); .Random.seed

RNGkind("Wich")# (partial string matching on 'kind')
p.WH <- c(30269, 30307, 30323)
a.WH <- c(  171,   172,	  170)
next.WHseed <- function(i.seed = .Random.seed[-1]) (a.WH * i.seed) \%\% p.WH
my.runif1 <- function(i.seed = .Random.seed)
  { ns <- next.WHseed(i.seed[-1]); sum(ns / p.WH) \%\% 1 }

## This shows how `runif(.)' works for Wichmann-Hill, using only R functions:
rs <- .Random.seed
(WHs <- next.WHseed(rs[-1]))
u <- runif(1)
all(next.WHseed(rs[-1]) == .Random.seed[-1])
all.equal(u, my.runif1(rs))


## ----
.Random.seed
ok <- RNGkind()
RNGkind("Super")#matches  "Super-Duper"
RNGkind()
.Random.seed # new, corresponding to  Super-Duper

## Reset:
RNGkind(ok[1])
}
\keyword{distribution}
\keyword{sysdata}
