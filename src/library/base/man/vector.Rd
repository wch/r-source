\name{vector}
\title{Vector Types}
\usage{
vector(type="logical", length=0)
as.vector(x,mode="any")
is.vector(x)
}
\alias{vector}
\alias{as.vector}
\alias{is.vector}
\description{
\code{vector} produces a vector of the given length and type.
Logical vector elements are initialized to
\code{FALSE}, numeric vector elements to \code{0}
and character vector elements to \code{""}.

\code{as.vector} attempts to coerce its argument
into a vector of mode \code{mode}
(the default is to coerce to whichever mode is most convenient).
The attributes of \code{x} are removed.

\code{is.vector} returns \code{TRUE} if \code{x}
is a vector (of mode logical, factor, integer, real or character) and
\code{FALSE} otherwise.
}
\keyword{classes}
