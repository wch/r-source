\name{tabulate}
\title{Tabulation for Vectors}
\usage{
tabulate(bin, nbin=max(1,bin))
}
\alias{tabulate}
\arguments{
    \item{bin}{a vector of integers, or a factor.}
    \item{nbins}{the number of bins to be used.}
}
\description{
\code{tabulate} takes the integer valued vector \code{bin} and counts
the number of times each integer occurs in it.  \code{tabulate} is
used as the basis of the \code{table} function.
}
\details{
    If \code{bin} is a factor, its internal integer representation
    is tabulated.
    If the elements of \code{bin} are not integers, they are rounded
    to the nearest integer.  Elements outside the range \code{1,\dots,
	nbin} are (silently) ignored in the tabulation.
}
\seealso{
\code{\link{factor}}, \code{\link{table}}.
}
\examples{
\testonly{tabulate(numeric(0))}
tabulate(c(2,3,5))
tabulate(c(2,3,3,5), nb = 10)
tabulate(c(-2,0,2,3,3,5), nb = 3)
tabulate(factor(letters[1:10]))
}
\keyword{arith}
