\name{drop}
\title{Drop Redundant Extent Information}
\usage{
drop(x)
}
\alias{drop}
\value{
If \code{x} is an object with a \code{dim} attribute
(e.g. a matrix or array), then \code{drop}, returns
an object like \code{x}, but with any extents of length
one removed.  Any accompanying \code{dimnames} attribute
is adjusted and returned with \code{x}.

Array subsetting typically performs this reduction,
but sometimes it is useful to invoke \code{drop} directly.
}
\keyword{array}
