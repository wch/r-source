\name{unique}
\alias{unique}
\alias{unique.default}
\alias{unique.data.frame}
\alias{unique.matrix}
\alias{unique.array}
\alias{unique.list}
\title{Extract Unique Elements}
\description{
  \code{unique} returns a vector, data frame or array like \code{x}
  but with duplicate  elements removed.
}
\usage{
unique(x, incomparables = FALSE, \dots)

\method{unique}{array}(x, incomparables = FALSE, MARGIN = 1, \dots)
}
\arguments{
  \item{x}{an atomic vector or a data frame or an array.}
  \item{incomparables}{a vector of values that cannot be compared.
    Currently, \code{FALSE} is the only possible value, meaning that all
    values can be compared.}
  \item{\dots}{arguments for particular methods.}
  \item{MARGIN}{the array margin to be held fixed: a single integer.}
}
\details{
  This is a generic function with methods for vectors, data frames and
  arrays (including matrices).

  The array method calculates for each element of the dimension
  specified by \code{MARGIN} if the remaining dimensions are identical
  to those for an earlier element (in row-major order).  This would most
  commonly be used to find unique rows (the default) or columns
  (with \code{MARGIN = 2}).
}
\value{
  An object of the same type of \code{x}. but if
  an element is equal to one with a smaller index, it is removed.
  Dimensions of arrays are not dropped.
}
\section{Warning}{
  The \code{list} method is potentially slow (\eqn{O(n^2}) in the
  current implementation.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{duplicated}} which gives the indices of duplicated
  elements.
}
\examples{
unique(c(3:5, 11:8, 8 + 0:5))
length(unique(sample(100, 100, replace=TRUE)))
## approximately 100(1 - 1/e) = 63.21

unique(iris)
}
\keyword{manip}
\keyword{logic}
