\name{sort}
\alias{sort}
\alias{is.unsorted}
\title{Sorting or Ordering Vectors}
\description{
  Sort (or \emph{order}) a numeric or complex vector (partially) into
  ascending order.
}
\usage{
sort(x, partial = NULL, na.last = NA, decreasing = FALSE,
     method = c("shell", "quick"), index.return = FALSE)
is.unsorted(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric or complex vector.}
  \item{partial}{a vector of indices for partial sorting.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
  \item{decreasing}{logical. Should the sort be increasing or decreasing?}
  \item{method}{character specifying the algorithm used.}
  \item{index.return}{logical indicating if the ordering index vector should
    be returned as well, for method \code{"quick"} only.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  If \code{partial} is not \code{NULL}, it is taken to contain indices
  of elements of \code{x} which are to be placed in their correct
  positions by partial sorting.  After the sort, the values specified in
  \code{partial} are in their correct position in the sorted array.  Any
  values smaller than these values are guaranteed to have a smaller
  index in the sorted array and any values which are greater are
  guaranteed to have a bigger index in the sorted array.

  The sort order for character vectors will depend on the collating
  sequence of the locale in use: see \code{\link{Comparison}}.

  \code{is.unsorted} returns a logical indicating if \code{x} is sorted
  increasingly, i.e. \code{is.unsorted(x)} is true if \code{any(x !=
    sort(x))} (and there are no \code{NA}s).

  \code{method = "shell"} uses Shellsort and was the only method
  before \R version 1.5.x (although improved there to an
  \eqn{O(n^{-4/3})} variant of Sedgewick (1996)).

  Method \code{"quick"} uses Singleton's Quicksort implementation and is
  only available when \code{x} is numeric, the sort is increasing,
  \code{partial} is \code{NULL} and \code{na.last = NA} or
  \code{index.return = FALSE}.  It is normally
  somewhat faster than Shellsort (perhaps twice as fast on vectors of
  length a million) but has poor performance in the rare worst case.
}
\value{
  For \code{sort} the sorted vector unless
  \code{method == "quick"} and \code{index.return} is true, when the result is
  a list with components named \code{x} and \code{ix} containing the
  sorted numbers and the ordering index vector.  In the latter case,
  ties may be reversed in the ordering, unlike \code{sort.list}, as
  quicksort is not stable.
}
\references{
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
data(swiss)
x <- swiss$Education[1:25]
x; sort(x); sort(x, partial = c(10, 15))
median # shows you another example for `partial'

stopifnot(!is.unsorted(sort(x)),
          !is.unsorted(LETTERS),
           is.unsorted(c(NA,1:3,2), na.rm = TRUE))

\testonly{## consistency check
 for(n in 1:20) {
    z <- rnorm(n)
    for(x in list(z, round(z,1))) { ## 2nd one has ties
       qxi <- sort(x,  method = "quick",  index.return = TRUE)
       stopifnot(qxi$x == sort(x, method = "shell"),
		 any(duplicated(x)) || qxi$ix == order(x),
		 x[qxi$ix] == qxi$x)
    }
 }
}
%%-- \longtest{ .. to be tested only in `extra-tests'
\dontrun{## Small speed comparison simulation:
N <- 2000
Sim <- 20
rep <- 50 # << adjust to your CPU
c1 <- c2 <- numeric(Sim)
for(is in 1:Sim){
  x <- rnorm(N)
  c1[is] <- system.time(for(i in 1:rep) sort(x, method = "shell"))[1]
  c2[is] <- system.time(for(i in 1:rep) sort(x, method = "quick"))[1]
  stopifnot(sort(x, meth = "s") == sort(x, meth = "q"))
}
100 * rbind(ShellSort = c1, QuickSort = c2)
cat("Speedup factor of quick sort():\n")
summary({qq <- c1 / c2; qq[is.finite(qq)]})

## A larger test
x <- rnorm(1e6)
system.time(sort(x, method = "shell"))
system.time(sort(x, method = "quick"))}
%%-- }
}
\keyword{univar}
\keyword{manip}
\keyword{arith}
