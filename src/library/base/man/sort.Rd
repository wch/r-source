\name{sort}
\alias{sort}
\alias{is.unsorted}
\title{Sorting or Ordering Vectors}
\description{
  Sort (or \emph{order}) a vector or factor (partially) into
  ascending (or descending) order.
}
\usage{
sort(x, partial = NULL, na.last = NA, decreasing = FALSE,
     method = c("shell", "quick"), index.return = FALSE)

is.unsorted(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric, complex, character or logical vector, or a factor.}
  \item{partial}{a vector of indices for partial sorting.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
  \item{decreasing}{logical.  Should the sort be increasing or decreasing?
     Not available for partial sorting.} 
  \item{method}{character specifying the algorithm used.}
  \item{index.return}{logical indicating if the ordering index vector should
    be returned as well; this is only available for a few cases, the default
    \code{na.last = NA} and full sorting of non-factors.}
  \item{na.rm}{logical.  Should missing values be removed?}
}
\details{
  If \code{partial} is not \code{NULL}, it is taken to contain indices
  of elements of \code{x} which are to be placed in their correct
  positions by partial sorting.  After the sort, the values specified in
  \code{partial} are in their correct position in the sorted array.  Any
  values smaller than these values are guaranteed to have a smaller
  index in the sorted array and any values which are greater are
  guaranteed to have a bigger index in the sorted array.  This is included
  for efficiency, and many of the options are not available for partial
  sorting.

  The sort order for character vectors will depend on the collating
  sequence of the locale in use: see \code{\link{Comparison}}.

  \code{is.unsorted} returns a logical indicating if \code{x} is sorted
  increasingly, i.e., \code{is.unsorted(x)} is true if \code{any(x !=
    sort(x))} (and there are no \code{NA}s).

  Method \code{"shell"} uses Shellsort (an \eqn{O(n^{4/3})} variant
  from Sedgewick (1996)).  If \code{x} has names a stable sort is used,
  so ties are not reordered.  (This only matters if names are present.)

  Method \code{"quick"} uses Singleton's Quicksort implementation and is
  only available when \code{x} is numeric (double or integer) and
  \code{partial} is \code{NULL}.  It is normally somewhat faster than
  Shellsort (perhaps twice as fast on vectors of length a million) but
  has poor performance in the rare worst case.
  (Peto's modification using a pseudo-random midpoint is used to make
  the worst case rarer.)  This is not a stable sort, and ties may be
  reordered.
}
\value{
  For \code{sort} the sorted vector unless
  \code{index.return} is true, when the result is
  a list with components named \code{x} and \code{ix} containing the
  sorted numbers and the ordering index vector.  In the latter case,
  if \code{method == "quick"} ties may be reversed in the ordering,
  unlike \code{sort.list}, as quicksort is not stable.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Sedgewick, R. (1986)
  A new upper bound for Shell sort.
  \emph{J. Algorithms} \bold{7}, 159--173.

  Singleton, R. C. (1969) An efficient algorithm for sorting with
  minimal storage: Algorithm 347.
  \emph{Communications of the ACM} \bold{12}, 185--187.
}
\seealso{
  \code{\link{order}}, \code{\link{rank}}.
}
\examples{
require(stats)
x <- swiss$Education[1:25]
x; sort(x); sort(x, partial = c(10, 15))
median # shows you another example for 'partial'

## illustrate 'stable' sorting (of ties):
sort(c(10:3,2:12), method = "sh", index=TRUE) # is stable
## $x : 2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 12
## $ix: 9  8 10  7 11  6 12  5 13  4 14  3 15  2 16  1 17 18 19
sort(c(10:3,2:12), method = "qu", index=TRUE) # is not
## $x : 2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 12
## $ix: 9 10  8  7 11  6 12  5 13  4 14  3 15 16  2 17  1 18 19
##        ^^^^^

\dontrun{## Small speed comparison simulation:
N <- 2000
Sim <- 20
rep <- 50 # << adjust to your CPU
c1 <- c2 <- numeric(Sim)
for(is in 1:Sim){
  x <- rnorm(N)
  c1[is] <- system.time(for(i in 1:rep) sort(x, method = "shell"),
                        gcFirst = TRUE)[1]
  c2[is] <- system.time(for(i in 1:rep) sort(x, method = "quick"),
                        gcFirst = TRUE)[1]
  stopifnot(sort(x, meth = "s") == sort(x, meth = "q"))
}
100 * rbind(ShellSort = c1, QuickSort = c2)
cat("Speedup factor of quick sort():\n")
summary({qq <- c1 / c2; qq[is.finite(qq)]})

## A larger test
x <- rnorm(1e6)
system.time(x1 <- sort(x, method = "shell"), gcFirst = TRUE)
system.time(x2 <- sort(x, method = "quick"), gcFirst = TRUE)
stopifnot(identical(x1, x2))
}}
\keyword{univar}
\keyword{manip}
\keyword{arith}
