\name{NA}
\title{Not Available / ``Missing'' Values}
\usage{
NA
is.na(x)
is.na.data.frame(x)
}
\alias{NA}
\alias{is.na}
\alias{is.na.data.frame}
\description{
\code{NA} is a logical constant of length 1 which contains
a missing value indicator.  \code{NA} can be freely coerced to any
other vector type.

The generic function \code{is.na} returns a logical vector
of the same ``form'' as its argument \code{x}, containing \code{TRUE}
for those elements marked
\code{NA} or \code{\link{NaN}} (!) and \code{FALSE} otherwise.
\code{dim}, \code{dimnames} and \code{names} attributes are preserved.

The method dispatching is C-internal, rather than via \code{\link{UseMethod}}.
}
\seealso{
  \code{\link{NaN}}, \code{\link{is.nan}}, etc.
  and the utility function \code{\link{complete.cases}}.

  \code{\link{na.action}}, \code{\link{na.omit}}, \code{\link{na.fail}}
  on how methods can be tuned to deal with missing values.
}
\examples{
is.na(c(1,NA))       #> F TRUE
is.na(paste(c(1,NA)))#> F FALSE
}
\keyword{NA}
\keyword{logic}
\keyword{manip}
