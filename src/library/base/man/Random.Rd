\name{Random}
\title{Random Number Generation}
\usage{
.Random.seed <- c(rng.kind, n1, n2, \dots)
save.seed <- .Random.seed

RNGkind(kind=NULL)
}
\alias{.Random.seed}
\alias{RNG}
\alias{RNGkind}
\description{
  \code{.Random.seed} is an integer vector, containing the random number
  generator (RNG) \bold{state} for random number generation in \R.

  \code{RNGkind} is a more friendly interface to query or set the kind
  of RNG in use. 
}
\arguments{
  \item{kind}{character or \code{NULL}.  If \code{kind} is a character
    string, set \R's RNG to the kind desired, if it's \code{NULL},
    return the currently used RNG.}
  \item{rng.kind}{integer code in \code{0:k} for the above \code{kind}.}
  \item{n1,n2,\dots}{integers. See the details for how many are required
    (which depends on \code{rng.kind}).}
}
\details{
  Currently available RNG kinds
  \itemize{
    \item ``Wichmann-Hill'': \code{.Random.seed[1] == 0} 
     
    The seed, \code{.Random.seed[-1] == r[1:3]} is an integer vector of
    length 3, where each \code{r[i]} is in \code{1:(p[i] - 1)}, where
    \code{p} is the length 3 vector of primes, \code{p = (30269, 30307,
      30323)}.
    The Wichmann-Hill generator has a cycle length of 6.9536e12 (=
    \code{prod(p-1)/4} ), see p.123 of Applied Statistics (1984) vol.33
    which corrects the original article.
    
    \item ``Marsaglia-Multicarry'': \code{.Random.seed[1] == 1} 

    A \emph{multiply-with-carry} RNG is used, as recommended by George
    Marsaglia in his post to the mailing list \file{sci.stat.math} on
    September 29, 1997.  It has a period of \eqn{> 2^60} and has passed
    all tests (according to Marsaglia).  The seed is two integers (all
    values allowed).

    \item ``Super-Duper'': \code{.Random.seed[1] == 2} 

    Marsaglia's famous Super-Duper from the 70's.  This is the original
    version which does \emph{not} pass the MTUPLE test of the Diehard
    battery.  It has a period of \eqn{\approx 4.6\times 10^{18}}{about
      4.6*10^18} for most initial seeds. The seed is two integers (all
    values allowed for the first seed: the second must be odd).
    
    We use the implementation as by Reeds et al.\ (1982--83), with the
    additional non-0 seed measure (see note below).
    
    The two seeds are the Tausworthe and Congruence long integers,
    respectively.  A one-to-one mapping to S's \code{.Random.seed[1:12]}
    is possible but we will not publish one, not least as this generator
    is \bold{not} exactly the same as that in recent versions of S-PLUS.
    
%%BUG 'stack ...' \item "Rand": \code{.Random.seed[1] == 3}
%%BUG 'stack ...'
%%BUG 'stack ...' This is the cheap Unix built-in generator which is not at all
%%BUG 'stack ...' recommended for simulation.  Further, you can only set, but never
%%BUG 'stack ...' retrieve the internal seed used.  Therefore, the value of
%%BUG 'stack ...' \code{.Random.seed} is of no relevance (but you can set it!).

% NOT YET:
% \item "Mersenne-Twister": \code{.Random.seed[1] == 4}
  }
 --- --- to be expanded --- ---
    
 ((Planned additions are
 ``Mersenne-Twister'',
 ``Knuth-TAOCP'' (from TAOCP, Vol.2, 3rd ed.,1997),
 ``Ecuyer-...'',
 ``Eichenauer-...''))
 
 \bold{Note}: If any of \code{.Random.seed[i]} (\eqn{i>1}) is set to
 \code{0}, it will be substituted with \code{1} in the next call to a
 random number generator, such as \code{\link{runif}}.
}
\value{
  \code{.Random.seed} is an \code{\link{integer}} vector whose first
  element \emph{codes} the kind of RNG and therefore is in \code{0:k}
  where {k+1} is the number of available RNGs.

  In the underlying C, \code{.Random.seed[-1]} is used as \code{unsigned
    long} (32 bits at least); in \R, whose \code{integer}s are C's
  \code{long}, \code{.Random.seed[i]} can therefore be negative for
  \eqn{i > 1}.
  
  \code{RNGkind} returns the RNG in use \emph{before} the call,
  invisibly if \code{kind} is not \code{NULL}.
}
\references{
  B. A. Wichmann and I. D. Hill (1982).
  \emph{Algorithm AS 183: An Efficient and Portable Pseudo-random Number
    Generator}, Applied Statistics, \bold{31}, 188-190; Remarks:
  \bold{34}, 198 and \bold{35}, 89.

  A. De Matteis and S. Pagnutti (1993).
  \emph{Long-range Correlation Analysis of the Wichmann-Hill Random
      Number Generator}, Statist. Comput., \bold{3}, 67-70.
  
  Marsaglia, G. (1997). \emph{A random number generator for C}. Discussion 
  paper, posting on usenet newsgroup \code{sci.stat.math}.
  
  Marsaglia, G. and Zaman, A. (1994). \emph{Some portable very-long-period 
  random number generators}. Computers in Physics, \bold{8}, 117-121. 
}
\note{
  Initially, there is no seed;  a new one is created, using
  ``Randomize''.  Hence, student exercises will each have different
  simulation results, by default.
}
\author{of RNGkind: Martin Maechler}

\seealso{\code{\link{runif}}, \code{\link{rnorm}}, \ldots.}
%	this is ./Uniform.Rd
\examples{
runif(1); .Random.seed; runif(1); .Random.seed
## If there is no seed,  a ``random'' new one is created:
rm(.Random.seed); runif(1); .Random.seed

RNGkind("Wich")# (partial string matching on 'kind')
p.WH <- c(30269, 30307, 30323)
a.WH <- c(  171,   172,   170)
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
RNGkind(ok)
}
\keyword{distribution}
\keyword{sysdata}
