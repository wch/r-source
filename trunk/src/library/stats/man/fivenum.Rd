\name{fivenum}
\alias{fivenum}
\title{Tukey Five-Number Summaries}
\description{
  Returns Tukey's five number summary (minimum, lower-hinge, median,
  upper-hinge, maximum) for the input data.
}
\usage{
fivenum(x, na.rm = TRUE)
}
\arguments{
  \item{x}{numeric, maybe including \code{\link{NA}}s and
    \eqn{\pm}{+/-}\code{\link{Inf}}s.}
  \item{na.rm}{logical; if \code{TRUE}, all \code{\link{NA}} and
    \code{\link{NaN}}s are dropped, before the statistics are computed.}
}
\value{
  A numeric vector of length 5 containing the summary information.  See
  \code{\link{boxplot.stats}} for more details.
}
\seealso{
  \code{\link{IQR}},
  \code{\link{boxplot.stats}},
  \code{\link{median}},
  \code{\link{quantile}},
  \code{\link{range}}.
}
\examples{
fivenum(c(rnorm(100),-1:1/0))
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

