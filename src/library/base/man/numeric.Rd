\name{numeric}
\title{Numeric Vectors}
\alias{numeric}
\alias{as.numeric}
\alias{is.numeric}
\usage{
numeric(length = 0)
as.numeric(x, \dots)
is.numeric(x)
}
\description{
  \code{numeric} creates a real vector of the specified length.  The
  elements of the vector are all equal to \code{0}.

  \code{as.numeric} attempts to coerce its argument to numeric type
  (either integer or real).

  \code{is.numeric} returns \code{TRUE} if its argument is of type real
  or type integer and \code{FALSE} otherwise.
}
\note{
  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.

  \code{as.numeric} for factors yields the codes underlying the factor
    levels.
}
\examples{
as.numeric(c("-.1"," 2.7 ","B")) # (-0.1, 2.7, NA)  +  warning
as.numeric(factor(5:10))
}
\keyword{classes}
\keyword{attribute}
