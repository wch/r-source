\name{NA}
\title{Not Available / ``Missing'' Values}
\usage{
NA
is.na(x)
}
\alias{NA}
\alias{is.na}
\description{
\code{NA} is a logical constant of length 1 which contains
a missing value indicator.  \code{NA} can be freely coerced to any
other vector type.

\code{is.na} takes a vector argument and returns a logical vector
of the same length containing \code{TRUE} for those elements marked
\code{NA} or \code{\link{NaN}} (!) and \code{FALSE} otherwise.
\code{dim}, \code{dimnames} and \code{names} attributes are preserved.
}
\seealso{
  \code{\link{NaN}}, \code{\link{is.nan}}, etc.
  and the utility function \code{\link{complete.cases}}.

  \code{\link{na.action}}, \code{\link{na.omit}}, \code{\link{na.fail}}
  on how methods can be tuned to deal with missing values.
}
\keyword{NA}
\keyword{logic}
\keyword{manip}
