\name{fivenum}
\title{Tukey Five-Number Summaries}
\usage{
fivenum(x, na.rm = TRUE)
}
\alias{fivenum}
\arguments{
\item{x}{numeric, maybe including \code{\link{NA}}'s and +/-\code{\link{Inf}}s.}
\item{na.rm}{logical; if \code{TRUE}, all \code{\link{NA}} and
  \code{\link{NaN}}s are dropped, before the statistics are computed.}
}
\value{
A five number summary (minimum, lower-hinge, median,
upper-hinge, maximum) for the (non \code{\link{NA}}) numbers in \code{x}.
}
\seealso{
\code{\link{IQR}}, \code{\link{boxplot.stats}}, \code{\link{median}},
\code{\link{quantile}}, \code{\link{range}}.
}
\examples{
fivenum(c(rnorm(100),-1:1/0))
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

