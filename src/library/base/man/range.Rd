\name{range}
\alias{range}
\alias{range.default}
\title{Range of Values}
\usage{
range(\dots, na.rm = FALSE)
range.default(\dots, na.rm = FALSE, finite = FALSE)
}
\arguments{
  \item{\dots}{any \code{\link{numeric}} objects.}
  \item{na.rm}{logical, indicating if \code{\link{NA}}'s should be
    omitted.}
  \item{finite}{logical, indicating if all non-finite elements should
    be omitted.}
}
\description{
  \code{range} returns a vector containing the minimum and maximum of
  all the given arguments.
}
\details{
  This is a generic function; currently, it has only a default method
  (\code{\link{range.default}}).

  It is also a member of the \code{Summary} group of functions, see
  \code{\link[base]{Methods}}.

  If \code{na.rm} is \code{FALSE}, \code{NA}
  and \code{NaN} values in any of the arguments will cause NA values
  to be returned, otherwise \code{NA} values are ignored.

  If \code{finite} is \code{TRUE}, the minimum
  and maximum of all finite values is computed, i.e.,
  \code{finite=TRUE} \emph{includes} \code{na.rm=TRUE}.

  A special situation occurs when there is no (after omission
  of \code{NA}s) nonempty argument left, see \code{\link{min}}.% Extremes.Rd
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{min}},
  \code{\link{max}},
  \code{\link[base]{Methods}}.
}
\examples{
print(r.x <- range(rnorm(100)))
diff(r.x) # the SAMPLE range

x <- c(NA, 1:3, -1:1/0); x
range(x)
range(x, na.rm = TRUE)
range(x, finite = TRUE)
}
\keyword{univar}
\keyword{arith}
