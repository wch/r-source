\name{rep}
\alias{rep}
\alias{rep.default}
\alias{rep.int}
\alias{rep.POSIXct}
\alias{rep.POSIXlt}
\alias{rep.Date}
\title{Replicate Elements of Vectors and Lists}
\description{
  \code{rep} replicates the values in \code{x}. It is a generic
  function, and the default method is described here.

  \code{rep.int} is a faster simplified version for the commonest case.
}
\usage{
rep(x, times, \dots)

\method{rep}{default}(x, times, length.out, each, \dots)

rep.int(x, times)
}
\arguments{
  \item{x}{a vector (of any mode including a list) or a pairlist or a
    \code{POSIXct} or \code{POSIXlt} or \code{date} object.}
  \item{times}{optional non-negative integer.  A vector giving the number of
    times to repeat each element if of length \code{length(x)}, or to
    repeat the whole vector if of length 1.}
  \item{length.out}{optional integer. The desired length of the
    output vector.}
  \item{each}{optional integer. Each element of \code{x} is
    repeated \code{each} times.}
  \item{\dots}{further arguments to be passed to or from other methods.}
}

\details{
  A least one of \code{times}, \code{length.out} and \code{each} must be
  specified, and normally exactly one is.  If \code{length.out} is
  given, \code{times} is ignored. If \code{each} is specified with
  either of the other two, its replication is performed first, and then
  that implied by \code{times} or \code{length.out}.
  
  If \code{times} consists of a single integer,
  the result consists of the values in
  \code{x} repeated this many times.
  If \code{times} is a vector of the same length as
  \code{x}, the result consists of \code{x[1]}
  repeated \code{times[1]} times,
  \code{x[2]} repeated \code{times[2]} times and so on.

  \code{length.out} may be given in place of \code{times},
  in which case \code{x} is repeated as many times as is
  necessary to create a vector of this length.

  Non-integer values of \code{times} will be truncated towards zero.
  If \code{times} is a computed quantity it is prudent to add a small fuzz.

  If \code{x} has length zero and \code{length.out} is supplied and is
  positive, the values are filled in using the extraction rules, that is
  by an \code{NA} of the appropriate class for an atomic vector and
  \code{NULL} for a list.  
}
\value{
  A vector of the same class as \code{x}.
}
\note{
  If the original vector has names, these are also replicated and so
  will almost always contain duplicates.  (In contrast, S strips the names.)
%
%  If \code{length.out} is used to extend the vector, the behaviour is
%  different from that of S-PLUS, which recycles the existing vector.

  Function \code{rep.int} is a simple case handled by internal code, and
  provided as a separate function purely for S compatibility.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{seq}}, \code{\link{sequence}}.
}
\examples{
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, len = 4)    # first 4 only.
rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications

rep(1, 40*(1-.8)) # length 7 on most platforms
rep(1, 40*(1-.8)+1e-7) # better

## replicate a list
fred <- list(happy = 1:10, name = "squash")
rep(fred, 5)

# date-time objects
x <- .leap.seconds[1:3]
rep(x, 2)
rep(as.POSIXlt(x), rep(2, 3))
}
\keyword{manip}
\keyword{chron}
