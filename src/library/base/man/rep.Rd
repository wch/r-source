\name{rep}
\alias{rep}
\alias{rep.int}
\title{Replicate Elements of Vectors and Lists}
\usage{
rep(x, times, length.out, each)
rep.int(x, times)
}
\arguments{
  \item{x}{a vector (of any mode including a list) or a pairlist.}
  \item{times}{non-negative integer.  A vector giving the number of
    times to repeat each element if of length \code{length(x)}, or to
    repeat the whole vector if of length 1.}
  \item{length.out}{integer.  (Optional.)  The desired length of the
    output vector.}
  \item{each}{optional integer. Each element of \code{x} is
    repeated \code{each} times.}
}
\description{
  \code{rep} replicates the values in \code{x}
  according to the values given in \code{times}
  and \code{length.out}.
}

\details{
  If \code{times} consists of a single integer,
  the result consists of the values in
  \code{x} repeated this many times.
  If \code{times} is a vector of the same length as
  \code{x}, the result consists of \code{x[1]}
  repeated \code{times[1]} times,
  \code{x[2]} repeated \code{times[2]} times and so on.

  \code{length.out} may be given in place of \code{times},
  in which case \code{x} is repeated as many times as is
  necessary to create a vector of this length.  If both
  \code{length.out} and \code{times} are specified, \code{times}
  determines the replication, and \code{length.out} can be used to
  truncate the output vector (or extend it by \code{NA}s).

  Non-integer values of \code{times} will be truncated towards zero.
  If \code{times} is a computed quantity it is prudent to add a small fuzz.
}
\note{
  If the original vector has names, these are also replicated and so
  will almost always contain duplicates.

  If \code{length.out} is used to extend the vector, the behaviour is
  different from that of S-PLUS, which recycles the existing vector.

  Function \code{rep.int} is a simple case handled by internal code, and
  provided as a separate function purely for S compatibility.
}
\seealso{
  \code{\link{seq}}, \code{\link{sequence}}.
}
\examples{
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, len = 4)  # first 4 only.
rep(1:4, each = 2, len = 10)   # 8 integers plus two NAs

rep(1, 40*(1-.8)) # length 7 on most platforms
rep(1, 40*(1-.8)+1e-7) # better

## replicate a list
fred <- list(happy = 1:10, name = "squash")
rep(fred, 5)

## more esoteric:
stopifnot(identical(rep(letters, 0), character(0)),
          identical(rep.int(1:2, 0), integer(0)))
}
\keyword{manip}
