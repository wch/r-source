\name{tabulate}
\title{Tabulation for Vectors}
\description{
  \code{tabulate} takes the integer-valued vector \code{bin} and counts
  the number of times each integer occurs in it. 
}
\usage{
tabulate(bin, nbins = max(1, bin))
}
\alias{tabulate}
\arguments{
    \item{bin}{a numeric vector (of positive integers), or a factor.}
    \item{nbins}{the number of bins to be used.}
}
\details{
   \code{tabulate} is used as the basis of the \code{\link{table}} function.
  
  If \code{bin} is a factor, its internal integer representation
  is tabulated.
  
  If the elements of \code{bin} are numeric but not integers,
  they are truncated to the nearest integer.
}
\value{
  An integer vector (without names).  There is a bin for each of the
  values \code{1, \dots, nbins}; values outside that range are (silently)
  ignored.
}
\seealso{
  \code{\link{table}}, \code{\link{factor}}.
}
\examples{
tabulate(c(2,3,5))
tabulate(c(2,3,3,5), nbins = 10)
tabulate(c(-2,0,2,3,3,5))  # -2 and 0 are ignored
tabulate(c(-2,0,2,3,3,5), nbins = 3)
tabulate(factor(letters[1:10]))
}
\keyword{arith}
