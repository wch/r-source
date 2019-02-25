% File src/library/base/man/Random.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{Random}
\alias{Random}
\alias{RNG}
\alias{RNGkind}
\alias{RNGversion}
\alias{set.seed}
\alias{.Random.seed}
\title{Random Number Generation}
\description{
  \code{.Random.seed} is an integer vector, containing the random number
  generator (RNG) \bold{state} for random number generation in \R.  It
  can be saved and restored, but should not be altered by the user.

  \code{RNGkind} is a more friendly interface to query or set the kind
  of RNG in use.

  \code{RNGversion} can be used to set the random generators as they
  were in an earlier \R version (for reproducibility).

  \code{set.seed} is the recommended way to specify seeds.
}
\usage{
\special{.Random.seed <- c(rng.kind, n1, n2, \dots)}

RNGkind(kind = NULL, normal.kind = NULL, sample.kind = NULL)
RNGversion(vstr)
set.seed(seed, kind = NULL, normal.kind = NULL, sample.kind = NULL)
}
\arguments{
  \item{kind}{character or \code{NULL}.  If \code{kind} is a character
    string, set \R's RNG to the kind desired.  Use \code{"default"} to
    return to the \R default.  See \sQuote{Details} for the
    interpretation of \code{NULL}.}
  \item{normal.kind}{character string or \code{NULL}.  If it is a character
    string, set the method of Normal generation.  Use \code{"default"}
    to return to the \R default.  \code{NULL} makes no change.}
  \item{sample.kind}{character string or \code{NULL}.  If it is a character
    string, set the method of discrete uniform generation (used in 
    \code{\link{sample}}, for instance).  Use \code{"default"} to return to 
    the \R default.  \code{NULL} makes no change.}
  \item{seed}{a single value, interpreted as an integer, or \code{NULL}
    (see \sQuote{Details}).}
  \item{vstr}{a character string containing a version number,
    e.g., \code{"1.6.2"}}
  \item{rng.kind}{integer code in \code{0:k} for the above \code{kind}.}
  \item{n1, n2, \dots}{integers.  See the details for how many are required
    (which depends on \code{rng.kind}).}
}
%% source and more detailed references:  ../../../main/RNG.c
\details{
  The currently available RNG kinds are given below.  \code{kind} is
  partially matched to this list.  The default is
  \code{"Mersenne-Twister"}.
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
    all tests (according to Marsaglia).  The seed is two integers (all
    values allowed).}

    \item{\code{"Super-Duper"}:}{
    Marsaglia's famous Super-Duper from the 70's.  This is the original
    version which does \emph{not} pass the MTUPLE test of the Diehard
    battery.  It has a period of \eqn{\approx 4.6\times 10^{18}}{about
      4.6*10^18} for most initial seeds.  The seed is two integers (all
    values allowed for the first seed: the second must be odd).

    We use the implementation by Reeds \emph{et al} (1982--84).

    The two seeds are the Tausworthe and congruence long integers,
    respectively.  A one-to-one mapping to S's \code{.Random.seed[1:12]}
    is possible but we will not publish one, not least as this generator
    is \bold{not} exactly the same as that in recent versions of S-PLUS.}

    \item{\code{"Mersenne-Twister"}:}{
      From Matsumoto and Nishimura (1998); code updated in 2002.
      A twisted GFSR with period
      \eqn{2^{19937} - 1}{2^19937 - 1} and equidistribution in 623
      consecutive dimensions (over the whole period).  The \sQuote{seed} is a
      624-dimensional set of 32-bit integers plus a current position in
      that set.

      R uses its own initialization method due to B. D. Ripley and is
      not affected by the initialization issue in the 1998 code of
      Matsumoto and Nishimura addressed in a 2002 update.
    }

    \item{\code{"Knuth-TAOCP-2002"}:}{
      A 32-bit integer GFSR using lagged Fibonacci sequences with
      subtraction.  That is, the recurrence used is
      \deqn{X_j = (X_{j-100} - X_{j-37}) \bmod 2^{30}%
      }{X[j] = (X[j-100] - X[j-37]) mod 2^30}
      and the \sQuote{seed} is the set of the 100 last numbers (actually
      recorded as 101 numbers, the last being a cyclic shift of the
      buffer).  The period is around \eqn{2^{129}}{2^129}.
    }

    \item{\code{"Knuth-TAOCP"}:}{
      An earlier version from Knuth (1997).

      The 2002 version was not backwards compatible with the earlier
      version: the initialization of the GFSR from the seed was altered.
      \R did not allow you to choose consecutive seeds, the reported
      \sQuote{weakness}, and already scrambled the seeds.

      Initialization of this generator is done in interpreted \R code
      and so takes a short but noticeable time.
    }
    \item{\code{"L'Ecuyer-CMRG"}:}{
      A \sQuote{combined multiple-recursive generator} from L'Ecuyer
      (1999), each element of which is a feedback multiplicative
      generator with three integer elements: thus the seed is a (signed)
      integer vector of length 6. The period is around
      \eqn{2^{191}}{2^191}.

      The 6 elements of the seed are internally regarded as 32-bit
      unsigned integers.  Neither the first three nor the last three
      should be all zero, and they are limited to less than
      \code{4294967087} and \code{4294944443} respectively.

      This is not particularly interesting of itself, but provides the
      basis for the multiple streams used in package \pkg{parallel}.
      % See \code{\link{RngStream}}.
    }

    \item{\code{"user-supplied"}:}{
      Use a user-supplied generator.  See \code{\link{Random.user}} for
      details.
    }
  }

  \code{normal.kind} can be \code{"Kinderman-Ramage"},
  \code{"Buggy Kinderman-Ramage"} (not for \code{set.seed}),
  \code{"Ahrens-Dieter"}, \code{"Box-Muller"}, \code{"Inversion"} (the
  default), or \code{"user-supplied"}.  (For inversion, see the
  reference in \code{\link{qnorm}}.)  The Kinderman-Ramage generator
  used in versions prior to 1.7.0 (now called \code{"Buggy"}) had several
  approximation errors and should only be used for reproduction of old
  results.  The \code{"Box-Muller"} generator is stateful as pairs of
  normals are generated and returned sequentially.  The state is reset
  whenever it is selected (even if it is the current normal generator)
  and when \code{kind} is changed.

  \code{sample.kind} can be \code{"Rounding"} or \code{"Rejection"},
  or partial matches to these.  The former was the default in versions
  prior to 3.6.0:  it made \code{\link{sample}} noticeably non-uniform
  on large populations, and should only be used for reproduction of old
  results.  See \PR{17494} for a discussion. 

  \code{set.seed} uses a single integer argument to set as many seeds
  as are required.  It is intended as a simple way to get quite different
  seeds by specifying small integer arguments, and also as a way to get
  valid seed sets for the more complicated methods (especially
  \code{"Mersenne-Twister"} and \code{"Knuth-TAOCP"}).  There is no
  guarantee that different values of \code{seed} will seed the RNG
  differently, although any exceptions would be extremely rare.  If
  called with \code{seed = NULL} it re-initializes (see \sQuote{Note})
  as if no seed had yet been set.

  The use of \code{kind = NULL}, \code{normal.kind = NULL} or 
  \code{sample.kind = NULL} in
  \code{RNGkind} or \code{set.seed} selects the currently-used
  generator (including that used in the previous session if the
  workspace has been restored): if no generator has been used it selects
  \code{"default"}.
}
\note{
  Initially, there is no seed; a new one is created from the current
  time and the process ID when one is required.  Hence different
  sessions will give different simulation results, by default.  However,
  the seed might be restored from a previous session if a previously
  saved workspace is restored.

  \code{.Random.seed} saves the seed set for the uniform random-number
  generator, at least for the system generators.  It does not
  necessarily save the state of other generators, and in particular does
  not save the state of the Box--Muller normal generator.  If you want
  to reproduce work later, call \code{set.seed} (preferably with
  explicit values for \code{kind} and \code{normal.kind}) rather than
  set \code{.Random.seed}.

  The object \code{.Random.seed} is only looked for in the user's
  workspace.

  Do not rely on randomness of low-order bits from RNGs.  Most of the
  supplied uniform generators return 32-bit integer values that are
  converted to doubles, so they take at most \eqn{2^{32}}{2^32} distinct
  values and long runs will return duplicated values (Wichmann-Hill is
  the exception, and all give at least 30 varying bits.)
}
\value{
  \code{.Random.seed} is an \code{\link{integer}} vector whose first
  element \emph{codes} the kind of RNG and normal generator. The lowest
  two decimal digits are in \code{0:(k-1)}
  where \code{k} is the number of available RNGs.  The hundreds
  represent the type of normal generator (starting at \code{0}), and
  the ten thousands represent the type of discrete uniform sampler.

  In the underlying C, \code{.Random.seed[-1]} is \code{unsigned};
  therefore in \R \code{.Random.seed[-1]} can be negative, due to
  the representation of an unsigned integer by a signed integer.

  \code{RNGkind} returns a three-element character vector of the RNG,
  normal and sample kinds selected \emph{before} the call, invisibly if 
  either argument is not \code{NULL}.  A type starts a session as the 
  default, and is selected either by a call to \code{RNGkind} or by setting
  \code{.Random.seed} in the workspace.

  \code{RNGversion} returns the same information as \code{RNGkind} about
  the defaults in a specific \R version.

  \code{set.seed} returns \code{NULL}, invisibly.
}
\references{
  Ahrens, J. H. and Dieter, U. (1973).
  Extensions of Forsythe's method for random sampling from the normal
  distribution.
  \emph{Mathematics of Computation}, \bold{27}, 927--937.

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
  (\code{set.seed}, storing in \code{.Random.seed}.)

  Box, G. E. P. and Muller, M. E. (1958).
  A note on the generation of normal random deviates.
  \emph{Annals of Mathematical Statistics}, \bold{29}, 610--611.
  \doi{10.1214/aoms/1177706645}.

  De Matteis, A. and Pagnutti, S. (1993).
  Long-range Correlation Analysis of the Wichmann-Hill Random Number
  Generator.
  \emph{Statistics and Computing}, \bold{3}, 67--70.
  \doi{10.1007/BF00153065}.

  Kinderman, A. J. and Ramage, J. G. (1976).
  Computer generation of normal random variables.
  \emph{Journal of the American Statistical Association}, \bold{71},
  893--896.
  \doi{10.2307/2286857}.

  Knuth, D. E. (1997).
  \emph{The Art of Computer Programming}.
  Volume 2, third edition.\cr
  Source code at \url{http://www-cs-faculty.stanford.edu/~knuth/taocp.html}.

  Knuth, D. E. (2002).
  \emph{The Art of Computer Programming}.
  Volume 2, third edition, ninth printing.

  L'Ecuyer, P. (1999).
  Good parameters and implementations for combined multiple recursive
  random number generators.
  \emph{Operations Research}, \bold{47}, 159--164.
  \doi{10.1287/opre.47.1.159}.

  Marsaglia, G. (1997).
  \emph{A random number generator for C}.
  Discussion paper, posting on Usenet newsgroup \code{sci.stat.math} on
  September 29, 1997.

  Marsaglia, G. and Zaman, A. (1994).
  Some portable very-long-period random number generators.
  \emph{Computers in Physics}, \bold{8}, 117--121.
  \doi{10.1063/1.168514}.

  Matsumoto, M. and Nishimura, T. (1998).
  Mersenne Twister: A 623-dimensionally equidistributed uniform
  pseudo-random number generator,
  \emph{ACM Transactions on Modeling and Computer Simulation},
  \bold{8}, 3--30.\cr
  Source code formerly at \code{http://www.math.keio.ac.jp/~matumoto/emt.html}.\cr
  Now see \url{http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/c-lang.html}.

  Reeds, J., Hubert, S. and Abrahams, M. (1982--4).
  C implementation of SuperDuper, University of California at Berkeley.
  (Personal communication from Jim Reeds to Ross Ihaka.)

  Wichmann, B. A.  and Hill, I. D. (1982).
  Algorithm AS 183: An Efficient and Portable Pseudo-random Number
  Generator.
  \emph{Applied Statistics}, \bold{31}, 188--190; Remarks:
  \bold{34}, 198 and \bold{35}, 89.
  \doi{10.2307/2347988}.
}
\author{of RNGkind: Martin Maechler. Current implementation, B. D. Ripley
  with modifications by Duncan Murdoch.}

\seealso{
  \code{\link{sample}} for random sampling with and without replacement.

  \link{Distributions} for functions for random-variate generation from
  standard distributions.
}
\examples{
require(stats)

## Seed the current RNG, i.e., set the RNG status
set.seed(42); u1 <- runif(30)
set.seed(42); u2 <- runif(30) # the same because of identical RNG status:
stopifnot(identical(u1, u2))

\donttest{## the default random seed is 626 integers, so only print a few
 runif(1); .Random.seed[1:6]; runif(1); .Random.seed[1:6]
 ## If there is no seed, a "random" new one is created:
 rm(.Random.seed); runif(1); .Random.seed[1:6]
}
ok <- RNGkind()
RNGkind("Wich")  # (partial string matching on 'kind')

## This shows how 'runif(.)' works for Wichmann-Hill,
## using only R functions:

p.WH <- c(30269, 30307, 30323)
a.WH <- c(  171,   172,   170)
next.WHseed <- function(i.seed = .Random.seed[-1])
  { (a.WH * i.seed) \%\% p.WH }
my.runif1 <- function(i.seed = .Random.seed)
  { ns <- next.WHseed(i.seed[-1]); sum(ns / p.WH) \%\% 1 }
set.seed(1998-12-04)# (when the next lines were added to the souRce)
rs <- .Random.seed
(WHs <- next.WHseed(rs[-1]))
u <- runif(1)
stopifnot(
 next.WHseed(rs[-1]) == .Random.seed[-1],
 all.equal(u, my.runif1(rs))
)

## ----
.Random.seed
RNGkind("Super") # matches  "Super-Duper"
RNGkind()
.Random.seed # new, corresponding to  Super-Duper

## Reset:
RNGkind(ok[1])

## ----
sum(duplicated(runif(1e6))) # around 110 for default generator
## and we would expect about almost sure duplicates beyond about
qbirthday(1 - 1e-6, classes = 2e9) # 235,000
}
\keyword{distribution}
\keyword{sysdata}
