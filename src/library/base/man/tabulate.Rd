\name{tabulate}
\title{Tabulation for Vectors}
\usage{
tabulate(bin, nbin=max(bin))
}
\alias{tabulate}
\description{
\code{tabulate} takes the integer valued vector \code{bin} and counts
the number of times each integer occurs in it.  \code{tabulate} is
used as the basis of the \code{table} function.
}
\seealso{
\code{\link{factor}}, \code{\link{table}}.
}
\examples{
tabulate(c(2,3,5))
tabulate(c(2,3,5), nb = 10)
}
\keyword{arith}
