% File src/library/base/man/rep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{rep}
\alias{rep}
\alias{rep.factor}
\alias{rep.int}
\alias{rep.POSIXct}
\alias{rep.POSIXlt}
\alias{rep.Date}
\alias{rep_len}

\title{Replicate Elements of Vectors and Lists}
\description{
  \code{rep} replicates the values in \code{x}.  It is a generic
  function, and the (internal) default method is described here.

  \code{rep.int} and \code{rep_len} are faster simplified versions for
  two common cases.  They are not generic.
}
\usage{
rep(x, \dots)

rep.int(x, times)

rep_len(x, length.out)
}
\arguments{
  \item{x}{a vector (of any mode including a list) or a factor or (for
    \code{rep} only) a \code{POSIXct} or \code{POSIXlt} or \code{Date}
    object; or an S4 object containing such an object.}
  \item{\dots}{further arguments to be passed to or from other methods.
    For the internal default method these can include:
    \describe{
      \item{\code{times}}{A integer vector giving the (non-negative) number of
        times to repeat each element if of length \code{length(x)}, or to
        repeat the whole vector if of length 1.  Negative or \code{NA}
	values are an error.}
      \item{\code{length.out}}{non-negative integer.  The desired length of the
        output vector.  Other inputs will be coerced to an integer
	vector and the first element taken.  Ignored if \code{NA} or invalid.}
      \item{\code{each}}{non-negative integer.  Each element of \code{x}
	is repeated \code{each} times.  Other inputs will be coerced to
	an integer vector and the first element taken.  Treated as
	\code{1} if \code{NA} or invalid.}
    }
  }
  \item{times}{see \code{\dots}.}
  \item{length.out}{non-negative integer: the desired length of the
    output vector.}
}

\details{
  The default behaviour is as if the call was
\preformatted{  rep(x, times = 1, length.out = NA, each = 1)
}.  Normally just one of the additional
  arguments is specified, but if \code{each} is specified with either
  of the other two, its replication is performed first, and then that
  implied by \code{times} or \code{length.out}.

  If \code{times} consists of a single integer, the result consists of
  the whole input repeated this many times.  If \code{times} is a
  vector of the same length as \code{x} (after replication by
  \code{each}), the result consists of \code{x[1]} repeated
  \code{times[1]} times, \code{x[2]} repeated \code{times[2]} times and
  so on.

  \code{length.out} may be given in place of \code{times},
  in which case \code{x} is repeated as many times as is
  necessary to create a vector of this length.  If both are given,
  \code{length.out} takes priority and \code{times} is ignored.

  Non-integer values of \code{times} will be truncated towards zero.
  If \code{times} is a computed quantity it is prudent to add a small
  fuzz or use \code{\link{round}}.  And analogously for \code{each}.

  If \code{x} has length zero and \code{length.out} is supplied and is
  positive, the values are filled in using the extraction rules, that is
  by an \code{NA} of the appropriate class for an atomic vector
  (\code{0} for raw vectors) and \code{NULL} for a list.
}
\value{
  An object of the same type as \code{x}.

  \code{rep.int} and \code{rep_len} return no attributes (except the
  class if returning a factor).

  The default method of \code{rep} gives the result names (which will
  almost always contain duplicates) if \code{x} had names, but retains
  no other attributes.
}
\note{
  Function \code{rep.int} is a simple case which was provided as a
  separate function partly for S compatibility and partly for speed
  (especially when names can be dropped).  The performance of \code{rep}
  has been improved since, but \code{rep.int} is still at least twice as
  fast when \code{x} has names.

  The name \code{rep.int} long precedes making \code{rep} generic.

  Function \code{rep} is a primitive, but (partial) matching of argument
  names is performed as for normal functions.

  For historical reasons \code{rep} (only) works on \code{NULL}: the
  result is always \code{NULL} even when \code{length.out} is positive.

  Although it has never been documented, these functions have always
  worked on \link{expression} vectors.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{seq}}, \code{\link{sequence}}, \code{\link{replicate}}.
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

## named factor
x <- factor(LETTERS[1:4]); names(x) <- letters[1:4]
x
rep(x, 2)
rep(x, each = 2)
rep.int(x, 2)  # no names
rep_len(x, 10)
}
\keyword{manip}
\keyword{chron}
