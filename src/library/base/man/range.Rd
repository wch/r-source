\name{range}
\alias{range}
\title{Range of Values}
\usage{
range(\dots, na.rm = FALSE)
}
\arguments{
  \item{\dots}{any \code{\link{numeric}} objects.}
  \item{na.rm}{logical, indicating if \code{\link{NA}}'s should be
      omitted.}
}
\description{
    \code{range} returns a vector containing the minimum and maximum of
    all the given arguments.  If \code{na.rm} is \code{FALSE}, \code{NA}
    and \code{NaN} values in any of the arguments will cause NA values
    to be returned, otherwise \code{NA} values are ignored.
}
\details{
    This is a generic function; currently, it has only a default method
    (\code{\link{range.default}}) which has an additional argument for
    indicating whether non-finite elements should be omitted.

    It is also a member of the Summary group of functions, see
    \code{\link{Methods}}.
}
\seealso{
    \code{\link{min}},
    \code{\link{max}},
    \code{\link{Methods}}.
}
\examples{
x <- c(NA, 1:3, -1:1/0); x
range(x)
range(x, na.rm = TRUE)
range(x, finite = TRUE)
}
\keyword{univar}
\keyword{arith}
