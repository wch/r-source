\name{apply}
\title{Apply Functions Over Array Margins}
\usage{
apply(x, MARGIN, FUN, \dots)
}
\alias{apply}
\arguments{
  \item{x}{the array to be used.}
  \item{MARGIN}{a vector giving the subscripts which the function will
    be applied over.
    \code{1} indicates rows, \code{2} indicates columns,
    \code{c(1,2)} indicates rows and columns.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\value{
  If each call to \code{FUN} returns a vector of length \code{n}, then
  \code{apply} returns an array of dimension \code{c(n,dim(x)[MARGIN])}
  if \code{n > 1}.  If \code{n} equals \code{1}, \code{apply} returns a
  vector if \code{MARGIN} has length 1 and an array of dimension
  \code{dim(x)[MARGIN]} otherwise.

  If the calls to \code{FUN} return vectors of different lengths,
  \code{apply} returns a list of length \code{dim(x)[MARGIN]}.
}
\seealso{
  \code{\link{lapply}}, \code{\link{tapply}}, \code{\link{sweep}}.
}
\examples{
## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

## Sort the columns of a matrix
apply(x, 2, sort)

ma <- matrix(c(1:4, 1, 6:8), nr = 2)
ma
apply(ma, 1, table)  #--> a list of length 2
}
\keyword{iteration}
\keyword{array}
