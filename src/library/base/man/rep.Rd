\name{rep}
\title{Replicate Elements}
\usage{
rep(x, times, length.out)
}
\alias{rep}
\description{
\code{rep} replicates the values in \code{x}
according to the values given in \code{times}
and \code{length.out}.

If \code{times} consists of a single integer,
the result consists of the values in
\code{x} repeated this many times.
If \code{times} is a vector of the same length as
\code{x}, the result consists of \code{x[1]}
repeated \code{times[1]} times,
\code{x[2]} repeated \code{times[2]} times and so on.

\code{length.out} may be given in place of \code{times},
in which case \code{x}
is repeated as many times as is necessary
to create a vector of this length.
}
\seealso{
\code{\link{seq}}, \code{\link{sequence}}.
}
\examples{
rep(1:4,2)
rep(1:4,c(2,2,2,2))
}
\keyword{manip}
