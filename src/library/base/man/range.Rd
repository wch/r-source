\name{range}
\title{Range of Values}
\usage{
range(\dots, na.rm = FALSE)
}
\alias{range}
\description{
  \code{range} returns a vector containing the minimum and maximum of
  all the values present in its arguments.  If \code{na.rm} is
  \code{FALSE}, an \code{NA} value in any of the arguments will cause
  values of \code{NA} to be returned, otherwise \code{NA} values are
  ignored.

  This definition of range should not be confused with the sample
  range.
}
\seealso{
  \code{\link{min}}, \code{\link{max}}.
}
\examples{
print(r.x <- range(rnorm(100)))
diff(r.x) # the SAMPLE range
}
\keyword{univar}
\keyword{arith}

