\name{order}
\title{Ordering Permutation}
\alias{order}
\alias{sort.list}
\description{
  \code{order} returns a permutation which rearranges its first
  argument into ascending or descending order, breaking ties by further
  arguments. \code{sort.list} is the same, using only one argument.
}
\usage{
order(\dots, na.last = TRUE, decreasing = FALSE)
sort.list(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
          method = c("shell", "quick", "radix"))
}
\arguments{
  \item{\dots}{a sequence of vectors, all of the same length.}
  \item{x}{a vector.}
  \item{partial}{vector of indices for partial sorting.}
  \item{decreasing}{logical. Should the sort order be increasing or
    decreasing?}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.
  }
  \item{method}{the method to be used: partial matches are allowed.}
}
\details{
  In the case of ties in the first vector, values in the second are used
  to break the ties.  If the values are still tied, values in the later
  arguments are used to break the tie (see the first example).
  The sort used is \emph{stable} (except for \code{method = "quick"}),
  so any unresolved ties will be left in their original ordering.

  When the ordering of ties does not matter in the one argument case
  (e.g., in \code{sort.list}), \code{method="quick"} may be much faster
  for long \code{x} vectors
  with many ties.  If the ordering does matter, \code{order(x, seq(along=x))} 
  may be faster than \code{sort.list(x)} with many ties.

  The default method is a good compromise.  Method \code{"shell"} is
  only supported for numeric \code{x} with \code{na.last=NA}, and is not
  stable.
  Method \code{"radix"} is only implemented for integer \code{x} with
  \code{0 <= x <= 1000} (or missing).  For such \code{x} it is very
  fast (and stable), and hence is ideal for sorting factors.

  \code{partial} is supplied for compatibility with other
  implementations of S, but no other values are accepted and ordering is
  always complete.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{sort}} and \code{\link{rank}}.}
\examples{
(ii <- order(x <- c(1,1,3:1,1:4,3), y <- c(9,9:1), z <-c(2,1:9)))
## 6  5  2  1  7  4 10  8  3  9
rbind(x,y,z)[,ii] # shows the reordering (ties via 2nd & 3rd arg)

## Suppose we wanted descending order on y. A simple solution is
rbind(x,y,z)[, order(x, -y, z)]
## For character vectors we can make use of rank:
cy <- as.character(y)
rbind(x,y,z)[, order(x, -rank(y), z)]

## rearrange matched vectors so that the first is in ascending order
x <- c(5:1, 6:8, 12:9)
y <- (x - 5)^2
o <- order(x)
rbind(x[o], y[o])

## tests of na.last
a <- c(4, 3, 2, NA, 1)
b <- c(4, NA, 2, 7, 1)
z <- cbind(a, b)
(o <- order(a, b)); z[o, ]
(o <- order(a, b, na.last = FALSE)); z[o, ]
(o <- order(a, b, na.last = NA)); z[o, ]

\dontrun{
x <- factor(sample(letters, 1e6, replace=TRUE))
system.time(o <- sort.list(x)) ## very slow 12 mins
stopifnot(!is.unsorted(x[o]))
system.time(o <- order(x, seq(along=x))) # 11 secs
stopifnot(!is.unsorted(x[o]))
system.time(o <- sort.list(unclass(x), method="quick", na.last=NA)) # 0.4 sec
stopifnot(!is.unsorted(x[o]))
system.time(o <- sort.list(x, method="radix")) # 0.04 sec
stopifnot(!is.unsorted(x[o]))
}}
\keyword{univar}
\keyword{manip}
