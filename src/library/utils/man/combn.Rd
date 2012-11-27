% File src/library/utils/man/combn.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{combn}
\alias{combn}
\title{Generate All Combinations of n Elements, Taken m at a Time}
\description{
  Generate all combinations of the elements of \code{x} taken \code{m}
  at a time.  If \code{x} is a positive integer, returns all
  combinations of the elements of \code{seq(x)} taken \code{m} at a
  time.  If argument \code{FUN} is not \code{NULL}, applies a function given
  by the argument to each point.  If simplify is FALSE,  returns
  a list; otherwise returns an \code{\link{array}}, typically a
  \code{\link{matrix}}.  \code{...} are passed unchanged to the
  \code{FUN} function, if specified.
}
\usage{
combn(x, m, FUN = NULL, simplify = TRUE, \dots)
}
\arguments{
  \item{x}{vector source for combinations, or integer \code{n} for
    \code{x <- \link{seq_len}(n)}.}
  \item{m}{number of elements to choose.}
  \item{FUN}{function to be applied to each combination; default
    \code{NULL} means the identity, i.e., to return the combination
    (vector of length \code{m}).}
  \item{simplify}{logical indicating if the result should be simplified
    to an \code{\link{array}} (typically a \code{\link{matrix}}); if
    FALSE, the function returns a \code{\link{list}}.  Note that when
    \code{simplify = TRUE} as by default, the dimension of the result is
    simply determined from \code{FUN(\var{1st combination})} (for
    efficiency reasons).  This will badly fail if \code{FUN(u)} is not of
    constant length.}
  \item{\dots}{optionally, further arguments to \code{FUN}.}
}
\value{
  a \code{\link{list}} or \code{\link{array}}, see the \code{simplify}
  argument above.  In the latter case, the identity
  \code{dim(combn(n, m)) == c(m, choose(n, m))} holds.
}
\references{
  Nijenhuis, A. and Wilf, H.S. (1978)
  \emph{Combinatorial Algorithms for Computers and Calculators};
  Academic Press, NY.
}
\author{Scott Chasalow wrote the original in 1994 for S;
  R package \CRANpkg{combinat} and documentation by Vince Carey
  \email{stvjc@channing.harvard.edu};
  small changes by the R core team, notably to return an array in all
  cases of \code{simplify = TRUE}, e.g., for \code{combn(5,5)}.
}
\seealso{
  \code{\link{choose}} for fast computation of the \emph{number} of
  combinations. \code{\link{expand.grid}} for creating a data frame from
  all combinations of factors or vectors.
}
\examples{
combn(letters[1:4], 2)
(m <- combn(10, 5, min))   # minimum value in each combination
mm <- combn(15, 6, function(x) matrix(x, 2, 3))
stopifnot(round(choose(10, 5)) == length(m),
          c(2,3, round(choose(15, 6))) == dim(mm))

## Different way of encoding points:
combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate, nbins = 4)

## Compute support points and (scaled) probabilities for a
## Multivariate-Hypergeometric(n = 3, N = c(4,3,2,1)) p.f.:
# table.mat(t(combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate, nbins = 4)))

## Assuring the identity
for(n in 1:7)
 for(m in 0:n) stopifnot(is.array(cc <- combn(n, m)),
                         dim(cc) == c(m, choose(n, m)))
}
\keyword{utilities}
\keyword{iteration}
