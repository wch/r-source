\name{rep}
\alias{rep}
\title{Replicate Elements of Vectors and Lists}
\usage{
rep(x, times, length.out)
}
\arguments{
  \item{x}{a vector (of any mode including a list) or a pairlist.}
  \item{times}{integer.  A vector giving the number of times to repeat
    each element. Either of length 1 or \code{length(x)}.}
  \item{length.out}{integer.  (Optional.)  The desired length of the
    output vector.}
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
}
\seealso{
  \code{\link{seq}}, \code{\link{sequence}}.
}
\examples{
rep(1:4, 2)
rep(1:4, c(2,2,2,2))     # same as above, length 8.
rep(1:4, c(2,1,2,1))
rep(1:4, c(2,2,2,2), 4)  # first 4 only.
rep(1:4, c(2,2,2,2), 10) # 8 intgers plus two NAs

rep(1, 40*(1-.8)) # length 7 on most platforms
rep(1, 40*(1-.8)+1e-7) # better

## replicate a list
fred <- list(happy = 1:10, name = "squash")
rep(fred, 5)
}
\keyword{manip}
