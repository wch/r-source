\name{NA}
\title{Not Available Value}
\usage{
NA
is.na(x)
as.na(x)
}
\alias{NA}
\alias{is.na}
\alias{as.na}
\description{
\code{NA} is a logical constant of length 1 which contains
a missing value indicator.  \code{NA} can be freely coerced to any
other vector type.

\code{as.na} ignores its argument and returns the value \code{NA}.

\code{is.na} takes a vector argument and returns a logical vector
of the same length containing \code{TRUE} for those elements marked
\code{NA} and \code{FALSE} otherwise.  \code{dim}, \code{dimnames} and
\code{names} attributes are preserved.
}
\seealso{
\code{\link{complete.cases}}.
}
\keyword{NA}
\keyword{logic}
\keyword{manip}
