\name{range}
\title{Range of Values}
\usage{
range(\dots, na.rm = FALSE, finite = FALSE)
}
\arguments{
  \item{\dots}{any \code{\link{numeric}} objects.}
  \item{na.rm}{logical, indicating if \code{\link{NA}}'s should be omitted.}
  \item{finite}{logical, indicating if all non-finite elements should be
    omitted.}

}
\alias{range}
\description{
  \code{range} returns a vector containing the minimum and maximum of
  all the values present in its first arguments, unless \code{na.rm} or
  \code{finite} is \code{TRUE}.
  If \code{na.rm} is \code{FALSE}, \code{NA} and \code{NaN} values in any of the
  arguments will cause NA values to be returned, otherwise
  \code{NA} values are ignored.
  If \code{finite} is \code{TRUE}, the minimum and maximum of all finite
  values is computed, i.e., \code{finite=TRUE} \emph{includes}
  \code{na.rm=TRUE}. 
}
\seealso{
  \code{\link{min}}, \code{\link{max}}.
}
\examples{
print(r.x <- range(rnorm(100)))
diff(r.x) # the SAMPLE range

x <- c(NA,1:3, -1:1/0); x
range(x)
range(x, na.rm = TRUE)
range(x, finite = TRUE)
}
\keyword{univar}
\keyword{arith}

