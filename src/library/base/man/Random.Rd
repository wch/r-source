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
  \code{.Random.seed} is an integer vector, containing the
  random number generator (RNG) \bold{state} for random number generation in \R.

  \code{RNGkind} is a more friendly interface to query or set the kind
  of RNG in use. 
}
\arguments{
    \item{kind}{character or \code{NULL}.  If \code{kind} is a character
	string, set \R's RNG to the kind desired, if it's \code{NULL},
	return the currently used RNG.}
    \item{rng.kind}{integer code in \code{0:k} for the above \code{kind}.}
    \item{n1,n2,\dots}{integers.}
}
\details{
        
 --- --- to be expanded --- ---
    
Available RNG kinds:

 \itemize{
     \item "Wichmann-Hill": \code{.Random.seed[1] == 0} 
     
     The seed, \code{.Random.seed[-1] == r[1:3]} is an integer vector of
     length 3, where each \code{r[i]} is in \code{1:p[i]}, where
     \code{p} is the length 3 vector of primes,
     \code{p = (30269, 30307, 30323)}. 
     The Wichmann-Hill generator has a cycle length of
     6.9536e12 (= \code{prod(p-1)/4} ), see p.123 of
     Applied Statistics (1984) vol.33 which corrects the original article.
    
    \item "Marsaglia-Multicarry"
    \item "Super-Duper"
    \item "Mersenne-Twister"
    \item "Rand"
 }

 \emph{Note}: If any of \code{.Random.seed[i]} is set to \code{0}, it
  will be substituted with \code{1}, in the next call to a random number
  generator, such as \code{\link{runif}}.

}
\value{
    \code{.Random.seed} is an \code{\link{integer}} vector whose first
    element \emph{codes} the kind of RNG and therefore is in \code{0:k}
    where {k+1} is the number of available RNGs.
    \cr
    In the underlying C, \code{.Random.seed[-1]} is used as \code{unsigned long}
    (32 bits at least); in \R, whose \code{integer}s are C's \code{long},
    \code{.Random.seed[i]} can therefore be negative for \eqn{i > 1}. 

    \code{RNGkind} returns the RNG in use \emph{before} the call, invisibly
    if \code{kind} isn't \code{NULL}.
}
\references{
  B.A. Wichmann and I. D. Hill (1982).
  \emph{Algorithm AS 183: An Efficient and Portable Pseudo-random Number
    Generator}, Applied Statistics, \bold{31}, 188-190; Remarks:
  \bold{34},p.198 and \bold{35}, p.89.

  A. De Matteis and S. Pagnutti (1993).
  \emph{Long-range Correlation Analysis of the Wichmann-Hill Random
    Number Generator}, Statist. Comput., \bold{3}, 67-70.
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

## This shows how `runif(.)'  for Wichmann-Hill, using only R functions:
rs <- .Random.seed
(WHs <- next.WHseed(rs[-1]))
u <- runif(1)
all(next.WHseed(rs[-1]) == .Random.seed[-1])
u == my.runif1(rs)


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
