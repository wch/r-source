\name{length}
\title{Length of a Vector or List}
\usage{
length(x)
length(x) <- n
}
\alias{length}
\description{
If \code{x} is a vector or list \code{length}
returns the length of \code{x}.
Otherwise, \code{length} returns \code{NA}.

The second form above can be used to reset
the length of a vector.
If a vector is shortened, extra values are discarded
and when a vector is lengthened, it is padded out to
its new length with \code{NA}s.
}
\keyword{attribute}

