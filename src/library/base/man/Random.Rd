\name{Random}
\title{Random Number Generation}
\usage{
.Random.seed <- c(n1, n2, n3)
}
\alias{.Random.seed}
\description{
  \code{.Random.seed} is an integer vector of length 3, containing the
  ``seed'' for all random number generation in \R.  The Wichmann-Hill
  generator is used which has a cycle length of 6.9536e12 (=
  \code{prod(p-1)/4} where \code{p} is the length 3 vector of primes,
  below), see p.123 of Applied Statistics (1984) vol.33 which corrects
  the original article.
}
\value{
  \code{.Random.seed == r[1:3]}, where  \code{r[i]} is in \code{1:p[i]},
  and \code{p = (30269, 30307, 30323)}.

  \emph{Note}: If any of \code{.Random.seed[i]} is set to \code{0}, it
  will be substituted with \code{1}, in the next call to a random number
  generator, such as \code{\link{runif}}.
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
\seealso{
  \code{\link{runif}}, \code{\link{rnorm}}, \ldots.
}
\examples{
runif(1); .Random.seed; runif(1); .Random.seed
## If there is no seed,  a ``random'' new one is created:
rm(.Random.seed); runif(1); .Random.seed

p.WH <- c(30269, 30307, 30323)
a.WH <- c(  171,   172,   170)
R.seed <- function(i.seed = .Random.seed) (a.WH * i.seed) \%\% p.WH
my.runif1 <- function(i.seed = .Random.seed)
  { ns <- R.seed(i.seed); sum(ns / p.WH) \%\% 1 }

## This shows how `runif(.)' works, just using  R functions :
rs <- .Random.seed
R.seed(rs); u <- runif(1); .Random.seed; c(u, my.runif1(rs))
}
\keyword{distribution}
\keyword{sysdata}
